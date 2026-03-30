package Simulator

final case class BackendNotReadyException(message: String) extends RuntimeException(message)
final case class BackendCrashedException(message: String)  extends RuntimeException(message)
final case class UnknownSignalException(path: String)      extends RuntimeException(s"Unknown signal: '$path'")
final case class InvalidSignalPathException(path: String)  extends RuntimeException(s"Invalid signal path: '$path'")
final case class InvalidValueException(message: String)    extends RuntimeException(message)
final case class InvalidStepException(cycles: Int)
    extends RuntimeException(s"step(cycles) requires cycles >= 1, got $cycles")
case object SessionClosedException                         extends RuntimeException("Simulation session is already closed")
