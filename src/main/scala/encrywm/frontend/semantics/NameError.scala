package encrywm.frontend.semantics

case class NameError(n: String) extends Error(s"Name $n is undefined.")
