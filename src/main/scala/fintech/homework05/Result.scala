package fintech.homework05

sealed trait Result[+T] {
  def get: T // only for test
  def map[U](f: T => U): Result[U]
  def flatMap[U](f: T => Result[U]): Result[U]
  override def toString: String = "Result"
}

final case class Success[T](value: T) extends Result[T] {
  def get: T = value
  def map[U](f: T => U): Result[U] = Success(f(value))
  def flatMap[U](f: T => Result[U]): Result[U] = f(value)
  override def toString: String = super.toString + "[Success(" + value + ")]"
}

final case class Error[T](message: String) extends Result[T] {
  def get: T = throw new NoSuchElementException
  def map[U](f: T => U): Result[U] = Error[Nothing](message)
  def flatMap[U](f: T => Result[U]): Result[U] = Error[Nothing](message)
  override def toString: String = super.toString + "[Error(" + message + ")]"
}