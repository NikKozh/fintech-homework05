package fintech.homework05

sealed trait Result[+T] {
  def hasError: Boolean
  def get: T // only for test
  override def toString: String = "Result"
}

final case class Success[T](value: T) extends Result[T] {
  def hasError: Boolean = false
  def get: T = value
  override def toString: String = super.toString + "[Success(" + value + ")]"
}

final case class Error[T](message: String) extends Result[T] {
  def hasError: Boolean = true
  def get: T = throw new NoSuchElementException
  override def toString: String = super.toString + "[Error(" + message + ")]"
}