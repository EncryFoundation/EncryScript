package encrywm.app

/* Created on 07.06.18 */
object Extensions {
  implicit class Traceable[T](val obj: T) extends AnyVal {
    def trace: T = { println(obj); obj}
    def trace[U]( u: U): T = { println(u); obj}
    def traceWith[S](reader: T => S ): T = { println(reader(obj)); obj}
  }
}
