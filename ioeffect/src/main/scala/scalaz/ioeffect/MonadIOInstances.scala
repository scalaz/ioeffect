package scalaz.ioeffect

import scalaz._
import Scalaz._

sealed abstract class MonadIOInstances extends MonadIOInstances1 {

  implicit val taskMonadIO: MonadIO[Task, Throwable] = new MonadIO[Task, Throwable] {
    override def liftIO[A](io: IO[Throwable, A])(implicit M: Monad[Task]): Task[A] = io
  }
}

sealed abstract class MonadIOInstances1 extends MonadIOInstances2 {

  implicit def ioMonadIO[E]: MonadIO[IO[E, ?], E] = new MonadIO[IO[E, ?], E] {
    override def liftIO[A](io: IO[E, A])(implicit M: Monad[IO[E, ?]]): IO[E, A] = io
  }
}

sealed abstract class MonadIOInstances2 {
  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit E: MonadIO[G, E], M1: Monad[G]): MonadIO[F, E] =
    new IsomorphismMonadIO[F, G, E] {
      override def G: MonadIO[G, E] = E
      override def M: Monad[G]      = M1
      override def iso: F <~> G     = D
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

abstract class IsomorphismMonadIO[F[_], G[_], E] extends MonadIO[F, E] {
  implicit def G: MonadIO[G, E]
  implicit def M: Monad[G]
  ////

  import Isomorphism._
  def iso: F <~> G

  override def liftIO[A](ioa: IO[E, A])(implicit F: Monad[F]): F[A] =
    iso.from(G.liftIO(ioa))
  ////
}
