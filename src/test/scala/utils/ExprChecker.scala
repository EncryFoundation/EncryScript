package utils

import fastparse.all._

trait ExprChecker {

  def check[T](rule: Parser[T], expected: T, s: String): T = {

    val parsed1 = (rule ~ End).parse(s)
    val stringResult = parsed1 match {
      case f: Parsed.Failure => throw new Exception(f.extra.traced.trace)
      case s: Parsed.Success[T] =>
        val result = s.value
        assert(result == expected)
        result
    }

    val chunkSizes = Seq(1, 4, 16, 64, 256, 1024)

    chunkSizes.foreach { cs =>
      val parsed2 = (rule ~ End).parseIterator(s.grouped(cs))
      parsed2 match {
        case f: Parsed.Failure => throw new Exception(f.extra.traced.trace)
        case s: Parsed.Success[T] =>
          val result = s.value
          assert(result == expected)
      }
    }

    stringResult
  }
}
