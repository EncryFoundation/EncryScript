package encrywm.backend

import cats.data.EitherT
import encrywm.backend.executor.error.ExecutionError
import monix.eval.Coeval

package object executor {

  type EvalResult[T] = EitherT[Coeval, ExecutionError, T]
}
