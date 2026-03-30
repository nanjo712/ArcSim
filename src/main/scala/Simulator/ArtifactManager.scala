package Simulator

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.time.Instant

private object ArtifactManager {
    def prepare(hwDialect: String, topName: String, config: SimConfig): SimArtifacts = {
        val base          = config.workDir.getOrElse(Files.createTempDirectory("arcsim-"))
        if (!Files.exists(base)) {
            Files.createDirectories(base)
        }
        val runDir        = if (config.workDir.isDefined) Files.createTempDirectory(base, "run-") else base
        val logDir        = runDir.resolve("logs")
        val inputMlirFile = runDir.resolve(s"$topName.hw.mlir")

        Files.createDirectories(logDir)
        Files.write(inputMlirFile, hwDialect.getBytes(StandardCharsets.UTF_8))
        Files.write(
          logDir.resolve("run.meta"),
          s"top=$topName\ncreated=${Instant.now()}\n".getBytes(StandardCharsets.UTF_8)
        )

        SimArtifacts(
          runDir        = runDir,
          inputMlirFile = inputMlirFile,
          logDir        = logDir,
          topName       = topName
        )
    }

    def cleanup(artifacts: SimArtifacts, config: SimConfig): Unit = {
        if (config.keepArtifacts) {
            return
        }
        if (!Files.exists(artifacts.runDir)) {
            return
        }
        val paths = Files.walk(artifacts.runDir).sorted(java.util.Comparator.reverseOrder())
        try {
            paths.forEach(Files.delete)
        } finally {
            paths.close()
        }
    }
}
