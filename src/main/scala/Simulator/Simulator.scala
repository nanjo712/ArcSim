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

object SimConfig {
  implicit val default: SimConfig = SimConfig()
}

final case class SimArtifacts(
  runDir: Path,
  inputMlirFile: Path,
  logDir: Path,
  topName: String,
)

private[Simulator] trait SimSession extends AutoCloseable {
  def poke(signal: Data, value: Data): Unit
  def peekData[T <: Data](signal: T): T
  def step(cycles: Int = 1): Unit
  override def close(): Unit
}

trait ArcSimulator {
  private[Simulator] def open[T <: Module](dutGen: => T, config: SimConfig = SimConfig()): SimSession = {
    openWithDut(dutGen, config)._2
  }

  final def simulate[T <: Module](dutGen: => T, config: SimConfig)(testFn: T => Unit): Unit = {
    val (dut, sim) = openWithDut(dutGen, config)
    try {
      SimSessionContext.withSession(sim) {
        testFn(dut)
      }
    } finally {
      sim.close()
    }
  }

  final def simulate[T <: Module](dutGen: => T)(testFn: T => Unit)(implicit config: SimConfig): Unit = {
    simulate(dutGen, config)(testFn)
  }

  implicit final class TestableData[T <: Data](private val signal: T) {
    def poke(value: T): Unit = SimSessionContext.currentOrThrow().poke(signal, value)

    def peek(): T = SimSessionContext.currentOrThrow().peekData(signal)

    def expect(expected: T): Unit = {
      val got = peek()
      SimExpect.expectFull(signal.pathName, got, expected)
    }

    def expectPartial(expected: T): Unit = {
      val got = peek()
      SimExpect.expectPartial(signal.pathName, got, expected)
    }
  }

  implicit final class TestableBool(private val signal: Bool) {
    def poke(value: Bool): Unit = SimSessionContext.currentOrThrow().poke(signal, value)
    def peek(): Bool = SimSessionContext.currentOrThrow().peekData(signal)
    def expect(value: Bool): Unit = {
      val got = peek()
      if (got.litValue != value.litValue) {
        throw FailedExpectException(s"expect failed at ${signal.pathName}: expected=${value.litValue} got=${got.litValue}")
      }
    }
  }

  implicit final class TestableClock(private val clock: Clock) {
    def step(cycles: Int = 1): Unit = SimSessionContext.currentOrThrow().step(cycles)
  }

  implicit final class TestableReset(private val reset: Reset) {
    def poke(value: Bool): Unit = SimSessionContext.currentOrThrow().poke(reset.asBool, value)
    def peek(): Bool = SimSessionContext.currentOrThrow().peekData(reset.asBool)
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
    try body
    finally {
      if (prev == null) local.remove() else local.set(prev)
    }
  }

  def currentOrThrow(): SimSession = {
    val s = local.get()
    if (s == null) throw BackendNotReadyException("No active simulation session in current thread")
    s
  }
}

private object SimExpect {
  def expectFull(path: String, got: Data, expected: Data): Unit = {
    compare(path, got, expected, partial = false)
  }

  def expectPartial(path: String, got: Data, expected: Data): Unit = {
    compare(path, got, expected, partial = true)
  }

  private def compare(path: String, got: Data, expected: Data, partial: Boolean): Unit = {
    (got, expected) match {
      case (g: Bool, e: Bool) =>
        if (partial && !e.isLit) return
        if (!e.isLit) throw FailedExpectException(s"expect failed at $path: expected is not literal")
        if (g.litValue != e.litValue) throw FailedExpectException(s"expect failed at $path: expected=${e.litValue} got=${g.litValue}")
      case (g: UInt, e: UInt) =>
        if (partial && !e.isLit) return
        if (!e.isLit) throw FailedExpectException(s"expect failed at $path: expected is not literal")
        if (g.litValue != e.litValue) throw FailedExpectException(s"expect failed at $path: expected=${e.litValue} got=${g.litValue}")
      case (g: SInt, e: SInt) =>
        if (partial && !e.isLit) return
        if (!e.isLit) throw FailedExpectException(s"expect failed at $path: expected is not literal")
        if (g.litValue != e.litValue) throw FailedExpectException(s"expect failed at $path: expected=${e.litValue} got=${g.litValue}")
      case (g: Record, e: Record) =>
        val ge = g.elements
        val ee = e.elements
        val keys = if (partial) ee.keySet else {
          if (ge.keySet != ee.keySet) throw FailedExpectException(s"record field mismatch at $path")
          ee.keySet
        }
        keys.foreach { k => compare(s"$path.$k", ge(k), ee(k), partial) }
      case (g: Vec[_], e: Vec[_]) =>
        if (!partial && g.length != e.length) throw FailedExpectException(s"vec length mismatch at $path")
        val n = if (partial) e.length else g.length
        var i = 0
        while (i < n) {
          compare(s"$path[$i]", g(i), e(i), partial)
          i += 1
        }
      case _ =>
        throw SignalTypeMismatchException(s"expect type mismatch at $path: got=${got.getClass.getSimpleName}, expected=${expected.getClass.getSimpleName}")
    }
  }
}

private object SimNaming {
  private val CircuitRegex: Regex = "(?m)^circuit\\s+([A-Za-z0-9_$.]+)\\s*:".r
  private val HwModuleRegex: Regex = "(?m)^\\s*hw\\.module\\s+@([A-Za-z0-9_$.]+)\\b".r

  def extractTopName(ir: String): Option[String] = {
    CircuitRegex.findFirstMatchIn(ir).map(_.group(1)).orElse(HwModuleRegex.findFirstMatchIn(ir).map(_.group(1)))
  }
}
