package fintech.homework05

sealed trait Result[+T] {
  def get: T // только для тестов
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
  def map[U](f: T => U): Result[U] = Error[Nothing](message)             // возможно, есть
  def flatMap[U](f: T => Result[U]): Result[U] = Error[Nothing](message) // способы получше?
  override def toString: String = super.toString + "[Error(" + message + ")]"
}