package scalaz.ioeffect

import scalaz.Monad
import scalaz._
import Scalaz._

/***
 * Monads in which `IO` computations may be embedded. Any monad built by applying a sequence of
 * monad transformers to the `IO` monad will be an instance of this class. Instances should satisfy the following laws,
 * which state that `liftIO` is a transformer of monads:
 *
 * liftIO . return = return
 * liftIO (m >>= f) = liftIO m >>= (liftIO . f)
 *
 * @tparam M - the monad in which to lift
 * @tparam E - the Error dependency corresponding with the IO to be lifted
 */
trait MonadIO[M[_], E] {

  /**
   * Lift a computation from the `IO` monad into `M`
   */
  def liftIO[A](io: IO[E, A])(implicit M: Monad[M]): M[A]

  trait MonadIOLaw {

    def leftIdentity[A](a: A)(implicit MA: Equal[M[A]], M: MonadIO[M, E], MM: Monad[M]): Boolean =
      MA.equal(a.pure[M], M.liftIO(IO.now(a)))

    def distributivity[A, B](
      f: A => IO[E, B],
      io: IO[E, A]
    )(implicit MB: Equal[M[B]], M: MonadIO[M, E], MM: Monad[M]): Boolean =
      MB.equal(M.liftIO(io.flatMap(f)), M.liftIO(io).flatMap(a => M.liftIO(f(a))))
  }
  def monadIOLaw = new MonadIOLaw {}

}

object MonadIO {
  def apply[M[_], E](implicit M: MonadIO[M, E]): MonadIO[M, E] = M

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit E: MonadIO[G, E], M1: Monad[G]): MonadIO[F, E] =
    new IsomorphismMonadIO[F, G, E] {
      override def G: MonadIO[G, E] = E
      override def M: Monad[G]      = M1
      override def iso: F <~> G     = D
    }
}
