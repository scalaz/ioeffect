package scalaz.ioeffect

import scalaz.Scalaz._
import scalaz._

abstract class MonadIOInstances {
  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: MonadIO[G]): MonadIO[F] =
    new IsomorphismMonadIO[F, G] {
      override def G: MonadIO[G] = E
      override def iso: F <~> G  = D
    }

  implicit def ioMonadIO[E]: MonadIO[IO[E, ?]] = new MonadIO[IO[E, ?]] {
    override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[IO[E, ?]]): IO[E, A] = io
  }

  implicit def identityTMonadIO[M[_]](implicit M: MonadIO[M]): MonadIO[IdT[M, ?]] = new MonadIO[IdT[M, ?]] {
    def liftIO[E, A](io: IO[E, A])(implicit M2: Monad[IdT[M, ?]]): IdT[M, A] = IdT(M.liftIO(io))
  }

  implicit def contTMonadIO[M[_], R](implicit M: MonadIO[M], M1: Bind[M]): MonadIO[ContT[M, R, ?]] =
    new MonadIO[ContT[M, R, ?]] {
      def liftIO[E, A](io: IO[E, A])(implicit M2: Monad[ContT[M, R, ?]]): ContT[M, R, A] =
        ContT(M.liftIO(io).flatMap)
    }

  implicit def readerTMonadIO[F[_], W](implicit F: MonadIO[F]): MonadIO[ReaderT[F, W, ?]] =
    new MonadIO[ReaderT[F, W, ?]] {
      override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[ReaderT[F, W, ?]]): ReaderT[F, W, A] =
        ReaderT(_ => F.liftIO(io))
    }

  implicit def stateTMonadIO[F[_], S](implicit F: MonadIO[F], F1: Functor[F]): MonadIO[StateT[F, S, ?]] =
    new MonadIO[StateT[F, S, ?]] {
      override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[StateT[F, S, ?]]): StateT[F, S, A] =
        StateT(s => F.liftIO(io).map(a => (s, a)))
    }

  implicit def writerTMonadIO[F[_], W](
    implicit F: MonadIO[F],
    F1: Functor[F],
    W: Monoid[W]
  ): MonadIO[WriterT[F, W, ?]] =
    new MonadIO[WriterT[F, W, ?]] {
      override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[WriterT[F, W, ?]]): WriterT[F, W, A] =
        WriterT(F.liftIO(io).map((W.zero, _)))
    }

  implicit def eitherTMonadIO[F[_], E0](implicit F: MonadIO[F], F1: Functor[F]): MonadIO[EitherT[F, E0, ?]] =
    new MonadIO[EitherT[F, E0, ?]] {
      override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[EitherT[F, E0, ?]]): EitherT[F, E0, A] =
        EitherT(F.liftIO(io).map(_.right[E0]))
    }

  implicit def optionTMonadIO[F[_]](implicit F: MonadIO[F], F1: Functor[F]): MonadIO[OptionT[F, ?]] =
    new MonadIO[OptionT[F, ?]] {
      override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[OptionT[F, _]]): OptionT[F, A] =
        OptionT(F.liftIO(io).map(Some.apply))
    }

  implicit def theseTMonadIO[F[_], E](implicit F: MonadIO[F], F1: Functor[F]): MonadIO[TheseT[F, E, ?]] =
    new MonadIO[TheseT[F, E, ?]] {
      override def liftIO[E, A](io: IO[E, A])(implicit M: Monad[TheseT[F, E, ?]]): TheseT[F, E, A] =
        TheseT(F.liftIO(io).map(\&/.That.apply))
    }

}

trait IsomorphismMonadIO[F[_], G[_]] extends MonadIO[F] {
  implicit def G: MonadIO[G]
  ////

  import Isomorphism._
  def iso: F <~> G

  override def liftIO[E, A](ioa: IO[E, A])(implicit F: Monad[F]): F[A] =
    iso.from(G.liftIO(ioa))
  ////
}
