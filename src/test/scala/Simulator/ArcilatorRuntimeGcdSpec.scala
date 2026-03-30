package Simulator

import ArcilatorResolver.ArcilatorResolver
import chisel3._
import gcd.DecoupledGcd
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ArcilatorRuntimeGcdSpec extends AnyFlatSpec with Matchers {
    behavior of "ArcilatorRuntimeBackend"

    it should "run gcd handshake loop with string signal paths" in {
        val version       = chisel3.BuildInfo.firtoolVersion.getOrElse("1.139.0")
        val arcilatorPath = ArcilatorResolver.resolve(version)

        ArcSimulator.simulate(
          new DecoupledGcd(16),
          SimConfig(
            arcilatorPathOverride = Some(arcilatorPath),
            keepArtifacts         = false
          )
        ) { sim =>
            sim.poke("reset", 1)
            sim.poke("input_valid", 0)
            sim.poke("output_ready", 0)
            sim.step(1)

            sim.poke("reset", 0)
            sim.step(1)

            val tests = Seq((48, 18), (21, 6), (0, 9), (7, 13), (100, 25))
            tests.foreach { case (a, b) =>
                sim.poke("input_bits_value1", a)
                sim.poke("input_bits_value2", b)
                sim.poke("input_valid", 1)

                var accepted = false
                var guard    = 0
                while (!accepted && guard < 1000) {
                    val ready = sim.peek("input_ready")
                    if (ready == 1) {
                        accepted = true
                    }
                    sim.step(1)
                    guard += 1
                }
                withClue(s"input handshake timeout for ($a, $b)") {
                    accepted mustBe true
                }

                sim.poke("input_valid", 0)
                sim.poke("output_ready", 1)

                var done = false
                guard = 0
                while (!done && guard < 2000) {
                    val valid = sim.peek("output_valid")
                    if (valid == 1) {
                        val got = sim.peek("output_bits_gcd")
                        got mustBe BigInt(a).gcd(BigInt(b))
                        done = true
                    }
                    sim.step(1)
                    guard += 1
                }
                withClue(s"output handshake timeout for ($a, $b)") {
                    done mustBe true
                }

                sim.poke("output_ready", 0)
                sim.step(1)
            }
        }
    }
}
