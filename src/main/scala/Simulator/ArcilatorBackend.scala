package Simulator

import ArcilatorResolver.ArcilatorResolver
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

    def poke(path: String, value: BigInt): Unit = {
        ensureOpen()
        validatePath(path)
        validateValue(value)
        adapter.poke(path, value)
    }

    def peek(path: String): BigInt = {
        ensureOpen()
        validatePath(path)
        adapter.peek(path)
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
