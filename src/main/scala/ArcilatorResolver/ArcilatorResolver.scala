package ArcilatorResolver

import chisel3._
import java.nio.file.{Files, Path, Paths}
import java.io.RandomAccessFile
import scala.sys.process._

object ArcilatorResolver {
    def resolve(version: String, cacheBaseDir: Path): Path = {
        val targetDir    = cacheBaseDir.resolve(version)
        val arcilatorBin = targetDir.resolve("bin").resolve("arcilator")

        if (Files.exists(arcilatorBin)) {
            return arcilatorBin.toAbsolutePath
        }

        Files.createDirectories(cacheBaseDir)
        Files.createDirectories(targetDir)

        val lockFile = targetDir.resolve(".install.lock")
        val raf      = new RandomAccessFile(lockFile.toFile, "rw")
        val channel  = raf.getChannel
        val lock     = channel.lock()

        try {
            if (Files.exists(arcilatorBin)) {
                return arcilatorBin.toAbsolutePath
            }

            val osName = System.getProperty("os.name").toLowerCase
            val osArch = System.getProperty("os.arch").toLowerCase

            val osStr =
                if (osName.contains("mac") || osName.contains("darwin")) "macos"
                else if (osName.contains("linux")) "linux"
                else throw new UnsupportedOperationException(s"Unsupported OS: $osName")

            val archStr =
                if (osArch.contains("amd64") || osArch.contains("x86_64")) "x64"
                else if (osArch.contains("aarch64") || osArch.contains("arm64")) "aarch64"
                else throw new UnsupportedOperationException(s"Unsupported architecture: $osArch")

            val tarballName = s"circt-full-shared-$osStr-$archStr.tar.gz"
            val downloadUrl = s"https://github.com/llvm/circt/releases/download/firtool-$version/$tarballName"
            val tarballPath = targetDir.resolve(tarballName)

            try {
                println(s"[ArcilatorResolver] Downloading circt from $downloadUrl")

                val curlCmd      = Seq("curl", "-f", "-L", "-o", tarballPath.toString, downloadUrl)
                val curlExitCode = curlCmd.!
                if (curlExitCode != 0) {
                    throw new RuntimeException(
                      s"Download failed, curl exit code: $curlExitCode. Please check your network connection or the version number."
                    )
                }

                println(s"[ArcilatorResolver] Download completed, extracting to: $targetDir")

                val tarCmd      = Seq("tar", "-xzf", tarballPath.toString, "-C", targetDir.toString, "--strip-components=1")
                val tarExitCode = tarCmd.!
                if (tarExitCode != 0) {
                    throw new RuntimeException(s"Extract failed, tar exit code: $tarExitCode")
                }

                if (!Files.exists(arcilatorBin)) {
                    throw new RuntimeException(
                      s"Extract completed but arcilator executable not found, expected path: $arcilatorBin"
                    )
                }

                println(s"[ArcilatorResolver] Environment is ready.")
                arcilatorBin.toAbsolutePath
            } finally {
                Files.deleteIfExists(tarballPath)
            }
        } catch {
            case e: Exception =>
                println(
                  s"[ArcilatorResolver] Error occurred while resolving Arcilator for version $version at path: $targetDir"
                )
                throw e
        } finally {
            lock.release()
            channel.close()
            raf.close()
        }
    }

    def resolve(version: String): Path = {
        val userHome     = System.getProperty("user.home")
        val cacheBaseDir = Paths.get(userHome, ".cache", "llvm-circt")
        resolve(version, cacheBaseDir)
    }
}
