package Simulator

import ArcilatorResolver.ArcilatorResolver
import chisel3._
import com.sun.jna.FunctionMapper
import com.sun.jna.Library
import com.sun.jna.Memory
import com.sun.jna.Native
import com.sun.jna.NativeLibrary
import com.sun.jna.Pointer
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.Locale
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.VecLiterals._
import scala.sys.process.Process
import scala.sys.process.ProcessIO
import scala.util.Try
import upickle.default._

private object ArcilatorBackend {
    def open(hwDialect: String, topName: String, config: SimConfig): SimSession = {
        val artifacts = ArtifactManager.prepare(hwDialect, topName, config)
        try {
            val arcilatorPath = resolveArcilator(config)
            runProbe(arcilatorPath, config, artifacts)
            compileRuntimeArtifacts(arcilatorPath, artifacts)
            val backend       = new ArcilatorRuntimeBackend(artifacts)
            new DefaultSimSession(backend, artifacts, config)
        } catch {
            case e: Throwable =>
                ArtifactManager.cleanup(artifacts, config)
                throw e
        }
    }

    private def resolveArcilator(config: SimConfig): Path = {
        config.arcilatorPathOverride.getOrElse {
            val version = config.arcilatorVersion.getOrElse {
                throw BackendNotReadyException("Unable to resolve arcilator version from Chisel BuildInfo")
            }
            ArcilatorResolver.resolve(version)
        }
    }

    private def runProbe(arcilatorPath: Path, config: SimConfig, artifacts: SimArtifacts): Unit = {
        val command = arcilatorPath.toString +: config.arcilatorProbeArgs
        val result  = ProcessRunner.run(command, artifacts.runDir)
        val logPath = artifacts.logDir.resolve("arcilator-probe.log")
        ProcessRunner.writeLog(logPath, command, result)
        if (result.exitCode != 0) {
            throw BackendNotReadyException(
              s"Arcilator probe failed with exitCode=${result.exitCode}. See log: $logPath"
            )
        }
    }

    private def compileRuntimeArtifacts(arcilatorPath: Path, artifacts: SimArtifacts): Unit = {
        val stateJson = artifacts.runDir.resolve(s"${artifacts.topName}.state.json")
        val llvmIr    = artifacts.runDir.resolve(s"${artifacts.topName}.ll")
        val sharedLib = artifacts.runDir.resolve(s"${artifacts.topName}.so")

        val compileCmd = Seq(
          arcilatorPath.toString,
          "--observe-ports",
          "--emit-llvm",
          "--state-file",
          stateJson.toString,
          "-o",
          llvmIr.toString,
          artifacts.inputMlirFile.toString
        )
        val compileRes = ProcessRunner.run(compileCmd, artifacts.runDir)
        val compileLog = artifacts.logDir.resolve("arcilator-compile.log")
        ProcessRunner.writeLog(compileLog, compileCmd, compileRes)
        if (compileRes.exitCode != 0) {
            throw BackendNotReadyException(s"Arcilator compile failed. See log: $compileLog")
        }

        val clangPath = ProcessRunner.requireBinary("clang")
        val clangCmd  = Seq(
          clangPath,
          "-shared",
          "-fPIC",
          "-O2",
          "-o",
          sharedLib.toString,
          llvmIr.toString
        )
        val clangRes  = ProcessRunner.run(clangCmd, artifacts.runDir)
        val clangLog  = artifacts.logDir.resolve("clang-build.log")
        ProcessRunner.writeLog(clangLog, clangCmd, clangRes)
        if (clangRes.exitCode != 0) {
            throw BackendNotReadyException(s"clang shared library build failed. See log: $clangLog")
        }
    }

}

private final class DefaultSimSession(adapter: ArcilatorRuntimeBackend, artifacts: SimArtifacts, config: SimConfig)
    extends SimSession {
  private var closed = false

  private def pokeRaw(path: String, value: BigInt): Unit = {
        ensureOpen()
        validatePath(path)
    validateValue(value)
    adapter.poke(path, value)
  }

  def poke(signal: chisel3.Data, value: chisel3.Data): Unit = {
    ensureOpen()
    if (signal == null || value == null) {
      throw SignalTypeMismatchException("poke(Data, Data) does not accept null")
    }
    if (!value.isLit) {
      throw SignalTypeMismatchException(
        s"poke(Data, Data) requires literal value, got non-literal ${value.getClass.getSimpleName}"
      )
    }
    PokeDataSupport.pokeAggregateOrLeaf(this, signal, value)
  }

  private def peekRaw(path: String): BigInt = {
    ensureOpen()
    validatePath(path)
    adapter.peek(path)
  }

  def peekData[T <: chisel3.Data](signal: T): T = {
    ensureOpen()
    PeekDataSupport.peekAs(signal, d => peekLeafBigInt(d)).asInstanceOf[T]
  }

  private def peekLeafBigInt(signal: chisel3.Data): BigInt = {
    signal match {
      case _: chisel3.Bool | _: chisel3.UInt | _: chisel3.SInt =>
      case _ =>
        throw SignalTypeMismatchException(
          s"peek(Data) supports Bool/UInt/SInt only, got ${signal.getClass.getSimpleName} at ${signal.pathName}"
        )
    }
    val path = adapter.resolvePath(signal, forWrite = false)
    peekRaw(path)
  }

    def step(cycles: Int): Unit = {
        ensureOpen()
        if (cycles < 1) {
            throw InvalidStepException(cycles)
        }
        adapter.step(cycles)
    }

    def close(): Unit = {
        if (!closed) {
            closed = true
            try {
                adapter.close()
            } finally {
                ArtifactManager.cleanup(artifacts, config)
            }
        }
    }

    private def ensureOpen(): Unit = {
        if (closed) {
            throw SessionClosedException
        }
    }

    private def validatePath(path: String): Unit = {
        if (path.trim.isEmpty) {
            throw InvalidSignalPathException(path)
        }
    }

  private def validateValue(value: BigInt): Unit = {
        if (value < 0) {
            throw InvalidValueException(s"Only non-negative values are supported in MVP, got $value")
  }
}

  private def validateKnownWidth(path: String, width: Int): Unit = {
    if (width <= 0) {
      throw UnknownSignalWidthException(path)
    }
  }

  private[Simulator] def pokeLeafSignal(signal: chisel3.Data, value: BigInt): Unit = {
    signal match {
      case b: chisel3.Bool =>
        if (value != 0 && value != 1) {
          throw InvalidValueException(s"Bool value must be 0 or 1 at ${b.pathName}, got $value")
        }
        val path = adapter.resolvePath(b, forWrite = true)
        pokeRaw(path, value)
      case u: chisel3.UInt =>
        validateKnownWidth(u.pathName, u.getWidth)
        val path = adapter.resolvePath(u, forWrite = true)
        pokeRaw(path, value)
      case s: chisel3.SInt =>
        validateKnownWidth(s.pathName, s.getWidth)
        val width = s.getWidth
        val min = -(BigInt(1) << (width - 1))
        val max = (BigInt(1) << (width - 1)) - 1
        if (value < min || value > max) {
          throw InvalidValueException(
            s"SInt value $value out of range for width=$width at ${s.pathName} (allowed [$min, $max])"
          )
        }
        val encoded = if (value >= 0) value else (BigInt(1) << width) + value
        val path = adapter.resolvePath(s, forWrite = true)
        pokeRaw(path, encoded)
      case other =>
        throw SignalTypeMismatchException(
          s"poke leaf supports Bool/UInt/SInt only, got ${other.getClass.getSimpleName} at ${other.pathName}"
        )
    }
  }
}

private object PokeDataSupport {
  def pokeAggregateOrLeaf(session: DefaultSimSession, signal: chisel3.Data, value: chisel3.Data): Unit = {
    (signal, value) match {
      case (s: chisel3.Bool, v: chisel3.Bool) =>
        session.pokeLeafSignal(s, if (v.litValue == 0) BigInt(0) else BigInt(1))
      case (s: chisel3.UInt, v: chisel3.UInt) =>
        session.pokeLeafSignal(s, v.litValue)
      case (s: chisel3.SInt, v: chisel3.SInt) =>
        val width = v.getWidth
        if (width <= 0) {
          throw UnknownSignalWidthException(v.pathName)
        }
        val raw = v.litValue
        val signBit = BigInt(1) << (width - 1)
        val signed = if ((raw & signBit) == 0) raw else raw - (BigInt(1) << width)
        session.pokeLeafSignal(s, signed)
      case (s: chisel3.Record, v: chisel3.Record) =>
        pokeRecord(session, s, v)
      case (s: chisel3.Vec[_], v: chisel3.Vec[_]) =>
        pokeVec(session, s, v)
      case (s, v) =>
        throw SignalTypeMismatchException(
          s"poke type mismatch: signal=${s.getClass.getSimpleName}(${s.pathName}), value=${v.getClass.getSimpleName}(${v.pathName})"
        )
    }
  }

  private def pokeRecord(session: DefaultSimSession, signal: chisel3.Record, value: chisel3.Record): Unit = {
    val sElems = signal.elements
    val vElems = value.elements
    val sNames = sElems.keys.toSet
    val vNames = vElems.keys.toSet
    if (sNames != vNames) {
      throw SignalTypeMismatchException(
        s"Record field mismatch at ${signal.pathName}: signal fields=${sNames.mkString(",")}, value fields=${vNames.mkString(",")}"
      )
    }
    sElems.foreach { case (name, sField) =>
      val vField = vElems(name)
      pokeAggregateOrLeaf(session, sField, vField)
    }
  }

  private def pokeVec(session: DefaultSimSession, signal: chisel3.Vec[_], value: chisel3.Vec[_]): Unit = {
    if (signal.length != value.length) {
      throw SignalTypeMismatchException(
        s"Vec length mismatch at ${signal.pathName}: signal length=${signal.length}, value length=${value.length}"
      )
    }
    var i = 0
    while (i < signal.length) {
      val sElem = signal(i).asInstanceOf[chisel3.Data]
      val vElem = value(i).asInstanceOf[chisel3.Data]
      pokeAggregateOrLeaf(session, sElem, vElem)
      i += 1
    }
  }
}

private object PeekDataSupport {
  def peekAs(signal: chisel3.Data, readLeaf: chisel3.Data => BigInt): chisel3.Data = {
    signal match {
      case b: chisel3.Bool =>
        val raw = readLeaf(b)
        if (raw == 0) false.B else if (raw == 1) true.B
        else throw BackendCrashedException(s"Bool signal ${b.pathName} has non-bool value $raw")
      case u: chisel3.UInt =>
        val width = u.getWidth
        if (width <= 0) throw UnknownSignalWidthException(u.pathName)
        readLeaf(u).U(width.W)
      case s: chisel3.SInt =>
        val width = s.getWidth
        if (width <= 0) throw UnknownSignalWidthException(s.pathName)
        val raw = readLeaf(s)
        val signBit = BigInt(1) << (width - 1)
        val signed = if ((raw & signBit) == 0) raw else raw - (BigInt(1) << width)
        signed.S(width.W)
      case r: chisel3.Record =>
        val clone = r.cloneType.asInstanceOf[chisel3.Record]
        val fields = r.elements.keys.toSeq.map { name =>
          val litChild = peekAs(r.elements(name), readLeaf)
          ((x: chisel3.Record) => x.elements(name) -> litChild)
        }
        clone.Lit(fields: _*)
      case v: chisel3.Vec[_] =>
        val clone = v.cloneType.asInstanceOf[chisel3.Vec[chisel3.Data]]
        val assigns = (0 until v.length).map { i =>
          i -> peekAs(v(i).asInstanceOf[chisel3.Data], readLeaf)
        }
        clone.Lit(assigns: _*)
      case other =>
        throw SignalTypeMismatchException(s"peek not supported for ${other.getClass.getSimpleName} at ${other.pathName}")
    }
  }
}

private final class ArcilatorRuntimeBackend(artifacts: SimArtifacts) {
    private val stateJsonPath = artifacts.runDir.resolve(s"${artifacts.topName}.state.json")
    private val sharedLibPath = artifacts.runDir.resolve(s"${artifacts.topName}.so")
    private val stateInfo     = StateJsonParser.parse(stateJsonPath)
    private val stateBuffer   = new Array[Byte](stateInfo.numStateBytes)
    private val evalFnName    = s"${artifacts.topName}_eval"
    private val evalInvoker   = NativeBridge.loadEval(sharedLibPath, evalFnName, stateInfo.numStateBytes)
  private var cycleCount    = BigInt(0)
  private var closed        = false

  def resolvePath(signal: chisel3.Data, forWrite: Boolean): String = {
    val raw = signal.pathName
    if (raw == null || raw.trim.isEmpty) {
      throw InvalidSignalPathException("")
    }

    val dotParts = raw.split('.').toSeq.filter(_.nonEmpty)
    val stripped = if (dotParts.length >= 2) dotParts.tail else dotParts
    val resolved = stripped.mkString("_").replace('[', '_').replace("]", "")
    if (resolved.trim.isEmpty) {
      throw InvalidSignalPathException(raw)
    }

    val lookup = if (forWrite) stateInfo.inputs else stateInfo.readable
    if (lookup.contains(resolved)) {
      resolved
    } else {
      val what = if (forWrite) "writable input" else "readable signal"
      throw UnknownSignalException(s"$raw (flattened: $resolved, no $what match)")
    }
  }

    def poke(path: String, value: BigInt): Unit = {
        ensureOpen()
        val signal = stateInfo.lookupInput(path)
        writeSignal(signal, value)
    }

    def peek(path: String): BigInt = {
        ensureOpen()
        if (path == "__cycle__") {
            return cycleCount
        }
        eval()
        val signal = stateInfo.lookupReadable(path)
        readSignal(signal)
    }

    def step(cycles: Int): Unit = {
        ensureOpen()
        val clock = stateInfo.findInput("clock").getOrElse {
            throw BackendNotReadyException("State JSON has no 'clock' input; step requires a clock signal")
        }
        var i     = 0
        while (i < cycles) {
            writeSignal(clock, BigInt(0))
            eval()
            writeSignal(clock, BigInt(1))
            eval()
            cycleCount += 1
            i += 1
        }
    }

    def close(): Unit = {
        if (!closed) {
            closed = true
            evalInvoker.close()
        }
    }

    private def eval(): Unit = {
        evalInvoker.eval(stateBuffer)
    }

    private def ensureOpen(): Unit = {
        if (closed) {
            throw SessionClosedException
        }
    }

    private def writeSignal(signal: SignalLayout, value: BigInt): Unit = {
        val max = (BigInt(1) << signal.numBits) - 1
        if (value < 0 || value > max) {
            throw InvalidValueException(
              s"Value $value does not fit signal '${signal.name}' (${signal.numBits} bits, max $max)"
            )
        }

        val byteCount = (signal.numBits + 7) / 8
        var v         = value
        var i         = 0
        while (i < byteCount) {
            stateBuffer(signal.offset + i) = (v & 0xff).toByte
            v                              = v >> 8
            i += 1
        }
        if (signal.numBits % 8 != 0 && byteCount > 0) {
            val last = signal.offset + byteCount - 1
            val mask = (1 << (signal.numBits % 8)) - 1
            stateBuffer(last) = (stateBuffer(last) & mask.toByte).toByte
        }
    }

    private def readSignal(signal: SignalLayout): BigInt = {
        val byteCount = (signal.numBits + 7) / 8
        var result    = BigInt(0)
        var i         = byteCount - 1
        while (i >= 0) {
            val b = stateBuffer(signal.offset + i) & 0xff
            result = (result << 8) | BigInt(b)
            i -= 1
        }
        if (signal.numBits % 8 == 0) result
        else result & ((BigInt(1) << signal.numBits) - 1)
    }
}

private final case class ProcessResult(exitCode: Int, stdout: String, stderr: String)

private object ProcessRunner {
    def run(command: Seq[String], cwd: Path): ProcessResult = {
        val stdout   = new ByteArrayOutputStream
        val stderr   = new ByteArrayOutputStream
        val io       = new ProcessIO(
          in => in.close(),
          out => {
              transfer(out, stdout)
              out.close()
          },
          err => {
              transfer(err, stderr)
              err.close()
          }
        )
        val exitCode = Process(command, cwd.toFile).run(io).exitValue()
        ProcessResult(
          exitCode = exitCode,
          stdout   = stdout.toString(StandardCharsets.UTF_8.name()),
          stderr   = stderr.toString(StandardCharsets.UTF_8.name())
        )
    }

    def writeLog(path: Path, command: Seq[String], result: ProcessResult): Unit = {
        val body =
            s"command=${command.mkString(" ")}\nexitCode=${result.exitCode}\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}\n"
        Files.write(path, body.getBytes(StandardCharsets.UTF_8))
    }

    def requireBinary(name: String): String = {
        val cmd = Seq("bash", "-lc", s"command -v $name")
        val res = run(cmd, Paths.get("."))
        if (res.exitCode != 0) {
            throw BackendNotReadyException(s"Required binary '$name' not found in PATH")
        }
        res.stdout.trim
    }

    private def transfer(in: java.io.InputStream, out: ByteArrayOutputStream): Unit = {
        val buffer = new Array[Byte](4096)
        var read   = in.read(buffer)
        while (read != -1) {
            out.write(buffer, 0, read)
            read = in.read(buffer)
        }
    }
}

private final case class SignalLayout(name: String, offset: Int, numBits: Int, signalType: String)

private final case class StateInfo(
  numStateBytes: Int,
  inputs:        Map[String, SignalLayout],
  readable: Map[String, SignalLayout]) {
    def lookupInput(name: String): SignalLayout = {
        inputs.getOrElse(name, throw UnknownSignalException(name))
    }

    def lookupReadable(name: String): SignalLayout = {
        readable.getOrElse(name, throw UnknownSignalException(name))
    }

    def findInput(name: String): Option[SignalLayout] = inputs.get(name)
}

private object StateJsonParser {
    private final case class StateModuleJson(
      name:          String,
      numStateBytes: Int,
      states:        Seq[StateEntryJson])
    private final case class StateEntryJson(
      name:    String,
      offset:  Int,
      numBits: Int,
      `type`:  String)

    private implicit val stateEntryRw:  ReadWriter[StateEntryJson]  = macroRW
    private implicit val stateModuleRw: ReadWriter[StateModuleJson] = macroRW

    def parse(path: Path): StateInfo = {
        val content = new String(Files.readAllBytes(path), StandardCharsets.UTF_8)
        val modules = Try(read[Seq[StateModuleJson]](content)).getOrElse {
            throw BackendNotReadyException(s"Failed to parse state JSON: $path")
        }
        val module  = modules.headOption.getOrElse {
            throw BackendNotReadyException(s"Empty state JSON in $path")
        }

        val layouts = module.states.map { item =>
            SignalLayout(
              name       = item.name,
              offset     = item.offset,
              numBits    = item.numBits,
              signalType = item.`type`.toLowerCase(Locale.ROOT)
            )
        }

        val inputs  = layouts.filter(_.signalType == "input").map(s => s.name -> s).toMap
        val outputs = layouts.filter(_.signalType == "output").map(s => s.name -> s).toMap
        val wires   = layouts.filter(_.signalType == "wire").map(s => s.name -> s).toMap
        StateInfo(module.numStateBytes, inputs, outputs ++ wires ++ inputs)
    }
}

private object NativeBridge {
    private trait EvalLibrary extends Library {
        def eval(ptr: Pointer): Unit
    }

    trait EvalInvoker {
        def eval(state: Array[Byte]): Unit
        def close():                  Unit
    }

    def loadEval(sharedLib: Path, fnName: String, stateBytes: Int): EvalInvoker = {
        val options = new java.util.HashMap[String, AnyRef]()
        options.put(
          Library.OPTION_FUNCTION_MAPPER,
          new FunctionMapper {
              override def getFunctionName(library: NativeLibrary, method: java.lang.reflect.Method): String = fnName
          }
        )
        val lib     = Native.load(sharedLib.toString, classOf[EvalLibrary], options)
        new EvalInvoker {
            private val memory = new Memory(stateBytes.toLong)

            override def eval(state: Array[Byte]): Unit = {
                if (state.length != stateBytes) {
                    throw BackendCrashedException(s"Unexpected state byte length ${state.length}, expected $stateBytes")
                }
                memory.write(0, state, 0, state.length)
                lib.eval(memory)
                memory.read(0, state, 0, state.length)
            }

            override def close(): Unit = {
                ()
            }
        }
    }
}
