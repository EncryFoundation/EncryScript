package encrywm.backend

import cats.data.EitherT
import monix.eval.Coeval

package object executor {

  type EvalResult[T] = EitherT[Coeval, ExecutionError, T]
}
