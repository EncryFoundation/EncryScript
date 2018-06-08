package encrywm.complex

import scala.util.Try

/* Created on 07.06.18 */
object ScriptSamples {
  import utils.ScriptGenerator._
  def base58const: Base58 = Base58("11BviJihxpMNf35SBy8e5SmWARsWCqJuRmLWk4NaFox")
  def lets(n: Int): String = (0 to n).map(i => Let(s"x$i", base58const).render).mkString("\n")
  val funcInvokeExample: String = Func.invoke("foo",
    Func.invoke("bar", "10", Base58("asddd")),
    Func.invoke("baz", "15")
  ).render

  val funcDefineExample: String = Func.define("foo", "Unit","a" -> "Int", "b" -> "String")(
    Let("x", Base58("11")),
    Let("x", Base58("11"))
  ).render

  val example1: String = block(
    Func.define("f", "Int", "a" -> "Int")("return (a + 1)"),
    Func.define("g", "Unit", "a" -> "Int")("unlock if f(a) == 1"),
    //    Func.invoke("f", "0")
    Func.invoke("f", "0")
  ).render

  val example2: String = block(
    Func.define("f", "Int", "a" -> "Int")("  return (a + 1)"),
    //    Func.invoke("f", "0")
    Func.invoke("f", "0")
  ).render

  val example3: String = block(
    Func.define("f", "Unit", "a" -> "Int")("  unlock if a + 1 == 1", "  return"),
    //    Func.invoke("f", "0")
    Func.invoke("f", "0")
  ).render

  val loooongInt: String = block(
    Let("a", Seq.fill(100)("1").mkString).render
  ).render

  val loooongString: String = block(
    Let("a", Str(Seq.fill(10000)("a").mkString)).render,
    "a"
  ).render

  def samplesFromFile(path: String, separator: String = "\n\n\n"): Seq[String] =
    Try(scala.io.Source.fromFile(path).mkString.split(separator).toSeq).getOrElse(Seq.empty)
}
