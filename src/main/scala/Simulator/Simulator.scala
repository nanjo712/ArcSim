package Simulator

import chisel3._
import circt.stage.ChiselStage
import java.nio.file.Path
import scala.util.matching.Regex

final case class SimConfig(
  workDir:               Option[Path]   = None,
  arcilatorVersion:      Option[String] = chisel3.BuildInfo.firtoolVersion,
  arcilatorPathOverride: Option[Path]   = None,
  arcilatorProbeArgs:    Seq[String]    = Seq("--version"),
  keepArtifacts:         Boolean        = false)

final case class SimArtifacts(
  runDir:        Path,
  inputMlirFile: Path,
  logDir:        Path,
  topName:       String)

trait SimSession extends AutoCloseable {
    def poke(path:   String, value: BigInt): Unit
    def peek(path:   String):                BigInt
    def step(cycles: Int = 1): Unit
    override def close():                    Unit
}

trait ArcSimulator {
    def open[T <: Module](dutGen: => T, config: SimConfig = SimConfig()): SimSession = {
        val hwDialect = ChiselStage.emitHWDialect(dutGen)
        val topName   = SimNaming.extractTopName(hwDialect).getOrElse("Top")
        ArcilatorBackend.open(hwDialect, topName, config)
    }

    final def simulate[T <: Module](dutGen: => T, config: SimConfig = SimConfig())(testFn: SimSession => Unit): Unit = {
        val sim = open(dutGen, config)
        try {
            testFn(sim)
        } finally {
            sim.close()
        }
    }
}

object ArcSimulator extends ArcSimulator

private object SimNaming {
    private val CircuitRegex:  Regex = "(?m)^circuit\\s+([A-Za-z0-9_$.]+)\\s*:".r
    private val HwModuleRegex: Regex = "(?m)^\\s*hw\\.module\\s+@([A-Za-z0-9_$.]+)\\b".r

    def extractTopName(ir: String): Option[String] = {
        CircuitRegex.findFirstMatchIn(ir).map(_.group(1)).orElse(HwModuleRegex.findFirstMatchIn(ir).map(_.group(1)))
    }
}
