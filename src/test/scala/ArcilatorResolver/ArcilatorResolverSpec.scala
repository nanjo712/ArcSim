package ArcilatorResolver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import java.nio.file.{Files, Path}

import chisel3._

class ArcilatorResolverSpec extends AnyFlatSpec with Matchers {
    behavior of "ArcilatorResolver"

    val testVersion = chisel3.BuildInfo.firtoolVersion.getOrElse("1.139.0")

    it should "download and extract the correct version of circt" in {
        val cacheBaseDir = Files.createTempDirectory("arcilator-resolver-test-")
        try {
            val arcilator = ArcilatorResolver.resolve(testVersion, cacheBaseDir)
            Files.exists(arcilator) shouldBe true
        } finally {
            deleteRecursively(cacheBaseDir)
        }
    }

    private def deleteRecursively(path: Path): Unit = {
        if (!Files.exists(path)) {
            return
        }
        val paths = Files.walk(path).sorted(java.util.Comparator.reverseOrder())
        try {
            paths.forEach(Files.delete)
        } finally {
            paths.close()
        }
    }
}
