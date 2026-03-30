package Simulator

import chisel3._
import circt.stage.ChiselStage
import java.nio.file.Path
import scala.util.matching.Regex

final case class SimConfig(
  workDir: Option[Path] = None,
  arcilatorVersion: Option[String] = chisel3.BuildInfo.firtoolVersion,
  arcilatorPathOverride: Option[Path] = None,
  arcilatorProbeArgs: Seq[String] = Seq("--version"),
  keepArtifacts: Boolean = false,
)

final case class SimArtifacts(
  runDir: Path,
  inputMlirFile: Path,
  logDir: Path,
  topName: String,
)

trait SimSession extends AutoCloseable {
  def poke(path: String, value: BigInt): Unit
  def poke(signal: Data, value: BigInt): Unit
  def peek(path: String): BigInt
  def peek(signal: Data): BigInt
  def step(cycles: Int = 1): Unit
  override def close(): Unit
}

trait ArcSimulator {
  def open[T <: Module](dutGen: => T, config: SimConfig = SimConfig()): SimSession = {
    openWithDut(dutGen, config)._2
  }

  final def simulate[T <: Module](dutGen: => T, config: SimConfig = SimConfig())(testFn: T => Unit): Unit = {
    val (dut, sim) = openWithDut(dutGen, config)
    try {
      SimSessionContext.withSession(sim) {
        testFn(dut)
      }
    } finally {
      sim.close()
    }
  }

  implicit final class DataSimOps(private val signal: Data) {
    def poke(value: BigInt): Unit = {
      SimSessionContext.currentOrThrow().poke(signal, value)
    }

    def poke(value: Int): Unit = poke(BigInt(value))
    def poke(value: Long): Unit = poke(BigInt(value))
    def poke(value: Boolean): Unit = poke(if (value) BigInt(1) else BigInt(0))

    def peek(): BigInt = {
      SimSessionContext.currentOrThrow().peek(signal)
    }
  }

  implicit final class ResetSimOps(private val reset: Reset) {
    def poke(value: BigInt): Unit = {
      SimSessionContext.currentOrThrow().poke(reset.asBool, value)
    }

    def poke(value: Int): Unit = poke(BigInt(value))
    def poke(value: Long): Unit = poke(BigInt(value))
    def poke(value: Boolean): Unit = poke(if (value) BigInt(1) else BigInt(0))

    def peek(): BigInt = {
      SimSessionContext.currentOrThrow().peek(reset.asBool)
    }
  }

  implicit final class ClockSimOps(private val clock: Clock) {
    def step(cycles: Int = 1): Unit = {
      SimSessionContext.currentOrThrow().step(cycles)
    }
  }

  private def openWithDut[T <: Module](dutGen: => T, config: SimConfig): (T, SimSession) = {
    var capturedDut: Option[T] = None
    val hwDialect = ChiselStage.emitHWDialect {
      val dut = dutGen
      capturedDut = Some(dut)
      dut
    }
    val topName = SimNaming.extractTopName(hwDialect).getOrElse("Top")
    val dut = capturedDut.getOrElse {
      throw BackendNotReadyException("Failed to capture elaborated DUT instance")
    }
    val sim = ArcilatorBackend.open(hwDialect, topName, config)
    (dut, sim)
  }
}

object ArcSimulator extends ArcSimulator

private object SimSessionContext {
  private val local = new ThreadLocal[SimSession]()

  def withSession[T](session: SimSession)(body: => T): T = {
    val prev = local.get()
    local.set(session)
    try {
      body
    } finally {
      if (prev == null) local.remove() else local.set(prev)
    }
  }

  def currentOrThrow(): SimSession = {
    val s = local.get()
    if (s == null) {
      throw BackendNotReadyException("No active simulation session in current thread")
    }
    s
  }
}

private object SimNaming {
  private val CircuitRegex: Regex = "(?m)^circuit\\s+([A-Za-z0-9_$.]+)\\s*:".r
  private val HwModuleRegex: Regex = "(?m)^\\s*hw\\.module\\s+@([A-Za-z0-9_$.]+)\\b".r

  def extractTopName(ir: String): Option[String] = {
    CircuitRegex.findFirstMatchIn(ir).map(_.group(1)).orElse(HwModuleRegex.findFirstMatchIn(ir).map(_.group(1)))
  }
}
