package Simulator

import ArcilatorResolver.ArcilatorResolver
import chisel3._
import chisel3.experimental.BundleLiterals._
import gcd.DecoupledGcd
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class ArcilatorRuntimeSpec extends AnyFlatSpec with Matchers with ArcSimulator {
    behavior of "ArcSimulator runtime"

    it should "use resolver by default and allow explicit arcilator override" in {
        val version      = chisel3.BuildInfo.firtoolVersion.getOrElse("1.139.0")
        val resolvedPath = ArcilatorResolver.resolve(version)

        simulate(new DecoupledGcd(16)) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)

            dut.input.bits.poke((new gcd.GcdInputBundle(16)).Lit(_.value1 -> 48.U, _.value2 -> 18.U))
            dut.input.valid.poke(true.B)

            var accepted = false
            var guard    = 0
            while (!accepted && guard < 1000) {
                if (dut.input.ready.peek().litToBoolean) accepted = true
                dut.clock.step()
                guard += 1
            }
            accepted mustBe true

            dut.input.valid.poke(false.B)
            dut.output.ready.poke(true.B)

            var done = false
            guard = 0
            while (!done && guard < 2000) {
                if (dut.output.valid.peek().litToBoolean) {
                    dut.output.bits.gcd.expect(6.U)
                    done = true
                }
                dut.clock.step()
                guard += 1
            }
            done mustBe true
        }

        simulate(
          new DecoupledGcd(16),
          SimConfig(arcilatorPathOverride = Some(resolvedPath), keepArtifacts = false)
        ) { dut =>
            dut.reset.poke(true.B)
            dut.clock.step()
            dut.reset.poke(false.B)

            dut.input.bits.poke((new gcd.GcdInputBundle(16)).Lit(_.value1 -> 21.U, _.value2 -> 6.U))
            dut.input.valid.poke(true.B)

            var accepted = false
            var guard    = 0
            while (!accepted && guard < 1000) {
                if (dut.input.ready.peek().litToBoolean) accepted = true
                dut.clock.step()
                guard += 1
            }
            accepted mustBe true

            dut.input.valid.poke(false.B)
            dut.output.ready.poke(true.B)

            var done = false
            guard = 0
            while (!done && guard < 2000) {
                if (dut.output.valid.peek().litToBoolean) {
                    val out = dut.output.bits.peek()
                    out.gcd.litValue mustBe 3
                    out.value1.litValue mustBe 21
                    out.value2.litValue mustBe 6
                    done = true
                }
                dut.clock.step()
                guard += 1
            }
            done mustBe true

        }
    }
}
