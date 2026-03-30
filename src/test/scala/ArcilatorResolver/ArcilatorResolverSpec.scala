package ArcilatorResolver

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.BeforeAndAfter
import java.nio.file.{Files, Path, Paths}

import chisel3._

class ArcilatorResolverSpec extends AnyFlatSpec with BeforeAndAfter {
    behavior of "ArcilatorResolver"

    val testVersion = chisel3.BuildInfo.firtoolVersion.getOrElse("firtool-1.62.0")

    after {
        // Clean up the cache directory after the test to avoid leaving large files on the system
        val userHome     = System.getProperty("user.home")
        val cacheBaseDir = Paths.get(userHome, ".cache", "llvm-circt")
        val targetDir    = cacheBaseDir.resolve(testVersion)
        if (Files.exists(targetDir)) {
            println(s"Cleaning up cache directory: $targetDir")
            Files.walk(targetDir).sorted(java.util.Comparator.reverseOrder()).forEach(Files.delete)
        }
    }

    it should "download and extract the correct version of circt" in {
        ArcilatorResolver.resolve(testVersion)
    }
}
