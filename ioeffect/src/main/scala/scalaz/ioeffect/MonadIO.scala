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

object MonadIO extends MonadIOInstances {
  def apply[M[_], E](implicit M: MonadIO[M, E]): MonadIO[M, E] = M

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit E: MonadIO[G, E], M1: Monad[G]): MonadIO[F, E] =
    new IsomorphismMonadIO[F, G, E] {
      override def G: MonadIO[G, E] = E
      override def M: Monad[G]      = M1
      override def iso: F <~> G     = D
    }
}

private[ioeffect] abstract class IsomorphismMonadIO[F[_], G[_], E] extends MonadIO[F, E] {
  implicit def G: MonadIO[G, E]
  implicit def M: Monad[G]

  import Isomorphism._
  def iso: F <~> G

  override def liftIO[A](ioa: IO[E, A])(implicit F: Monad[F]): F[A] =
    iso.from(G.liftIO(ioa))
}

private[ioeffect] sealed abstract class MonadIOInstances extends MonadIOInstances1 {

  implicit val taskMonadIO: MonadIO[Task, Throwable] = new MonadIO[Task, Throwable] {
    override def liftIO[A](io: IO[Throwable, A])(implicit M: Monad[Task]): Task[A] = io
  }

  implicit def identityTMonadIO[M[_], E](implicit M: MonadIO[M, E], M1: Monad[M]): MonadIO[IdT[M, ?], E] =
    new MonadIO[IdT[M, ?], E] {
      def liftIO[A](io: IO[E, A])(implicit M2: Monad[IdT[M, ?]]): IdT[M, A] = IdT(M.liftIO(io))
    }

  implicit def contTMonadIO[M[_], R, E](implicit M: MonadIO[M, E], M1: Monad[M]): MonadIO[ContT[M, R, ?], E] =
    new MonadIO[ContT[M, R, ?], E] {
      def liftIO[A](io: IO[E, A])(implicit M2: Monad[ContT[M, R, ?]]): ContT[M, R, A] =
        ContT(M.liftIO(io).flatMap)
    }

  implicit def readerTMonadIO[F[_], W, E](implicit F: MonadIO[F, E], M: Monad[F]): MonadIO[ReaderT[F, W, ?], E] =
    new MonadIO[ReaderT[F, W, ?], E] {
      override def liftIO[A](io: IO[E, A])(implicit M1: Monad[ReaderT[F, W, ?]]): ReaderT[F, W, A] =
        ReaderT(_ => F.liftIO(io))
    }

  implicit def stateTMonadIO[F[_], S, E](implicit F: MonadIO[F, E], M: Monad[F]): MonadIO[StateT[F, S, ?], E] =
    new MonadIO[StateT[F, S, ?], E] {
      override def liftIO[A](io: IO[E, A])(implicit M1: Monad[StateT[F, S, ?]]): StateT[F, S, A] =
        StateT(s => F.liftIO(io).map(a => (s, a)))
    }

  implicit def writerTMonadIO[F[_], W, E](
    implicit F: MonadIO[F, E],
    M: Monad[F],
    W: Monoid[W]
  ): MonadIO[WriterT[F, W, ?], E] =
    new MonadIO[WriterT[F, W, ?], E] {
      override def liftIO[A](io: IO[E, A])(implicit M1: Monad[WriterT[F, W, ?]]): WriterT[F, W, A] =
        WriterT(F.liftIO(io).map((W.zero, _)))
    }

  implicit def eitherTMonadIO[F[_], E0, E](implicit F: MonadIO[F, E], M: Monad[F]): MonadIO[EitherT[F, E0, ?], E] =
    new MonadIO[EitherT[F, E0, ?], E] {
      override def liftIO[A](io: IO[E, A])(implicit M1: Monad[EitherT[F, E0, ?]]): EitherT[F, E0, A] =
        EitherT(F.liftIO(io).map(_.right[E0]))
    }

  implicit def optionTMonadIO[F[_], E](implicit F: MonadIO[F, E], M: Monad[F]): MonadIO[OptionT[F, ?], E] =
    new MonadIO[OptionT[F, ?], E] {
      override def liftIO[A](io: IO[E, A])(implicit M1: Monad[OptionT[F, ?]]): OptionT[F, A] =
        OptionT(F.liftIO(io).map(Some.apply))
    }

  implicit def theseTMonadIO[F[_], E0, E](implicit F: MonadIO[F, E], M: Monad[F]): MonadIO[TheseT[F, E0, ?], E] =
    new MonadIO[TheseT[F, E0, ?], E] {
      override def liftIO[A](io: IO[E, A])(implicit M1: Monad[TheseT[F, E0, ?]]): TheseT[F, E0, A] =
        TheseT(F.liftIO(io).map(\&/.That.apply))
    }

}

private[ioeffect] sealed abstract class MonadIOInstances1 {

  implicit def ioMonadIO[E]: MonadIO[IO[E, ?], E] = new MonadIO[IO[E, ?], E] {
    override def liftIO[A](io: IO[E, A])(implicit M: Monad[IO[E, ?]]): IO[E, A] = io
  }
}
