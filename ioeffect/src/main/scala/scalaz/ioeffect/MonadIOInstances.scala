package scalaz.ioeffect

import scalaz._
import Scalaz._

sealed abstract class MonadIOInstances {

  implicit def identityTMonadIO[M[_]](implicit M: MonadIO[M]): MonadIO[IdT[M, ?]] = new MonadIO[IdT[M, ?]] {
    override def point[A](a: => A): IdT[M, A]                            = IdT(M.point(a))
    override def bind[A, B](fa: IdT[M, A])(f: A => IdT[M, B]): IdT[M, B] = fa.flatMap(f)
    def liftIO[E, A](io: IO[E, A]): IdT[M, A]                            = IdT(M.liftIO(io))
  }

  implicit def contTMonadIO[M[_], R](implicit M: MonadIO[M]): MonadIO[ContT[M, R, ?]] = new MonadIO[ContT[M, R, ?]] {
    override def point[A](a: => A): ContT[M, R, A]                                      = ContT.point(a)
    override def bind[A, B](fa: ContT[M, R, A])(f: A => ContT[M, R, B]): ContT[M, R, B] = fa.flatMap(f)
    def liftIO[E, A](io: IO[E, A]): ContT[M, R, A] =
      ContT(M.liftIO(io).flatMap)
  }

  implicit def readerTMonadIO[F[_], W](implicit F: MonadIO[F]): MonadIO[ReaderT[F, W, ?]] =
    new MonadIO[ReaderT[F, W, ?]] {
      override def liftIO[E, A](io: IO[E, A]): ReaderT[F, W, A] = ReaderT(_ => F.liftIO(io))
      override def bind[A, B](fa: ReaderT[F, W, A])(f: A => ReaderT[F, W, B]): ReaderT[F, W, B] =
        fa.flatMap(f)
      override def point[A](a: => A): ReaderT[F, W, A] = ReaderT(_ => F.point(a))
    }

  implicit def stateTMonadIO[F[_], S](implicit F: MonadIO[F]): MonadIO[StateT[F, S, ?]] = new MonadIO[StateT[F, S, ?]] {
    override def liftIO[E, A](io: IO[E, A]): StateT[F, S, A]                               = StateT(s => F.map(F.liftIO(io))(a => (s, a)))
    override def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)
    override def point[A](a: => A): StateT[F, S, A]                                        = StateT(s => F.point((s, a)))
  }

  implicit def writerTMonadIO[F[_], W](implicit F: MonadIO[F], W: Monoid[W]): MonadIO[WriterT[F, W, ?]] =
    new MonadIO[WriterT[F, W, ?]] {
      override def liftIO[E, A](io: IO[E, A]): WriterT[F, W, A] = WriterT(F.liftIO(io).map((W.zero, _)))
      override def bind[A, B](fa: WriterT[F, W, A])(f: A => WriterT[F, W, B]): WriterT[F, W, B] =
        fa.flatMap(f)
      override def point[A](a: => A): WriterT[F, W, A] = WriterT(F.point((W.zero, a)))
    }

  implicit def writerTMonadIO[F[_], E0](implicit F: MonadIO[F]): MonadIO[EitherT[F, E0, ?]] =
    new MonadIO[EitherT[F, E0, ?]] {
      override def liftIO[E, A](io: IO[E, A]): EitherT[F, E0, A] =
        EitherT(F.liftIO(io).map(_.right[E0]))
      override def bind[A, B](fa: EitherT[F, E0, A])(f: A => EitherT[F, E0, B]): EitherT[F, E0, B] =
        fa.flatMap(f)
      override def point[A](a: => A): EitherT[F, E0, A] =
        EitherT(F.point(a.right[E0]))
    }

}
