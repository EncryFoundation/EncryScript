package encrywm.utils

class Stack[T] {

  var stack: List[T] = List.empty

  def currentOpt: Option[T] = stack.headOption

  def lastOpt: Option[T] = stack.lastOption

  def push(e: T): Unit = {
    stack = e :: stack
  }

  def popHead(): Unit = {
    stack = stack.drop(1)
  }
}
