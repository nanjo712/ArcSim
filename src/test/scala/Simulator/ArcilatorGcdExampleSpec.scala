package Simulator

import ArcilatorResolver.ArcilatorResolver
import chisel3._
import circt.stage.ChiselStage
import gcd.DecoupledGcd
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import scala.sys.process.Process
import scala.sys.process.ProcessIO

class ArcilatorGcdExampleSpec extends AnyFlatSpec with Matchers {
    behavior of "Arcilator command flow"

    it should "compile GCD HW dialect into LLVM IR and state JSON" in {
        val version       = chisel3.BuildInfo.firtoolVersion.getOrElse("1.139.0")
        val arcilatorPath = ArcilatorResolver.resolve(version)

        val hwDialect = ChiselStage.emitHWDialect(new DecoupledGcd(16))
        val runDir    = Files.createTempDirectory("arcilator-gcd-")

        val inputMlir = runDir.resolve("DecoupledGcd.hw.mlir")
        val outputLl  = runDir.resolve("DecoupledGcd.ll")
        val stateJson = runDir.resolve("DecoupledGcd.state.json")
        val logFile   = runDir.resolve("arcilator.log")

        Files.write(inputMlir, hwDialect.getBytes(StandardCharsets.UTF_8))

        val command = Seq(
          arcilatorPath.toString,
          "--observe-ports",
          "--emit-llvm",
          "--state-file",
          stateJson.toString,
          "-o",
          outputLl.toString,
          inputMlir.toString
        )

        val result = run(command, runDir)
        val log    =
            s"command=${command.mkString(" ")}\nexitCode=${result.exitCode}\nstdout:\n${result.stdout}\nstderr:\n${result.stderr}\n"
        Files.write(logFile, log.getBytes(StandardCharsets.UTF_8))

        withClue(s"arcilator failed, log: $logFile") {
            result.exitCode mustBe 0
        }

        Files.exists(outputLl) mustBe true
        Files.exists(stateJson) mustBe true
        Files.size(outputLl) must be > 0L
        Files.size(stateJson) must be > 0L
    }

    private def run(command: Seq[String], cwd: Path): CommandResult = {
        val stdout = new ByteArrayOutputStream
        val stderr = new ByteArrayOutputStream
        val io     = new ProcessIO(
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
        CommandResult(
          exitCode,
          stdout.toString(StandardCharsets.UTF_8.name()),
          stderr.toString(StandardCharsets.UTF_8.name())
        )
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

private final case class CommandResult(exitCode: Int, stdout: String, stderr: String)
