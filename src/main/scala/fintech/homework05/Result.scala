package fintech.homework05

sealed trait Result[+T] {
  override def toString: String = "Result"
}
final case class Success[T](value: T) extends Result[T] {
  override def toString: String = super.toString + "[Success(" + value + ")]"
}
final case class Error[T](message: String) extends Result[T] {
  override def toString: String = super.toString + "[Error(" + message + ")]"
}