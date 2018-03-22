package encrywm.backend

import cats.data.EitherT
import monix.eval.Coeval

package object evaluator {

  type EvalResult[T] = EitherT[Coeval, EvaluationError, T]
}
