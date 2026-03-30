package Simulator

import ArcilatorResolver.ArcilatorResolver
import chisel3._
import gcd.DecoupledGcd
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ArcilatorRuntimeGcdSpec extends AnyFlatSpec with Matchers with ArcSimulator {
  behavior of "ArcilatorRuntimeBackend"

  it should "run gcd handshake loop with official-like poke/peek API" in {
    val version = chisel3.BuildInfo.firtoolVersion.getOrElse("1.139.0")
    val arcilatorPath = ArcilatorResolver.resolve(version)

    simulate(
      new DecoupledGcd(16),
      SimConfig(
        arcilatorPathOverride = Some(arcilatorPath),
        keepArtifacts = false,
      ),
    ) { dut =>
      dut.reset.poke(true)
      dut.input.valid.poke(false)
      dut.output.ready.poke(false)
      dut.clock.step()

      dut.reset.poke(false)
      dut.clock.step()

      val tests = Seq((48, 18), (21, 6), (0, 9), (7, 13), (100, 25))
      tests.foreach { case (a, b) =>
        dut.input.bits.value1.poke(a)
        dut.input.bits.value2.poke(b)
        dut.input.valid.poke(true)

        var accepted = false
        var guard    = 0
        while (!accepted && guard < 1000) {
          val ready = dut.input.ready.peek()
          if (ready == 1) {
            accepted = true
          }
          dut.clock.step()
          guard += 1
        }
        withClue(s"input handshake timeout for ($a, $b)") {
          accepted mustBe true
        }

        dut.input.valid.poke(false)
        dut.output.ready.poke(true)

        var done = false
        guard = 0
        while (!done && guard < 2000) {
          val valid = dut.output.valid.peek()
          if (valid == 1) {
            val got = dut.output.bits.gcd.peek()
            got mustBe BigInt(a).gcd(BigInt(b))
            done = true
          }
          dut.clock.step()
          guard += 1
        }
        withClue(s"output handshake timeout for ($a, $b)") {
          done mustBe true
        }

        dut.output.ready.poke(false)
        dut.clock.step()
      }
    }
  }
}
