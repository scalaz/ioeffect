// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

abstract class IOInstances extends IOInstances1 {
  // cached for efficiency
  implicit val taskInstances: MonadError[Task, Throwable] with BindRec[Task] with Plus[Task] =
    new IOMonadError[Throwable] with IOPlus[Throwable]

  implicit val taskParAp: Applicative[Task.Par] = new IOParApplicative[Throwable]
}

sealed abstract class IOInstances1 extends IOInstance2 {
  implicit def ioInstances[E]: MonadError[IO[E, ?], E] with BindRec[IO[E, ?]] with Bifunctor[IO] with Plus[IO[E, ?]] =
    new IOMonadError[E] with IOPlus[E] with IOBifunctor

  implicit def ioParAp[E]: Applicative[IO.Par[E, ?]] = new IOParApplicative[E]
}

sealed abstract class IOInstance2 {
  implicit def ioMonadPlus[E: Monoid]: MonadPlus[IO[E, ?]] with BindRec[IO[E, ?]] = new IOMonadPlus[E]
}

private class IOMonad[E] extends Monad[IO[E, ?]] with BindRec[IO[E, ?]] {
  override def map[A, B](fa: IO[E, A])(f: A => B): IO[E, B]         = fa.map(f)
  override def point[A](a: => A): IO[E, A]                          = IO.point(a)
  override def bind[A, B](fa: IO[E, A])(f: A => IO[E, B]): IO[E, B] = fa.flatMap(f)
  override def tailrecM[A, B](f: A => IO[E, A \/ B])(a: A): IO[E, B] =
    f(a).flatMap(_.fold(tailrecM(f), point(_)))
}

private class IOMonadError[E] extends IOMonad[E] with MonadError[IO[E, ?], E] {
  override def handleError[A](fa: IO[E, A])(f: E => IO[E, A]): IO[E, A] = fa.catchAll(f)
  override def raiseError[A](e: E): IO[E, A]                            = IO.fail(e)
}

// lossy, throws away errors using the "first success" interpretation of Plus
private trait IOPlus[E] extends Plus[IO[E, ?]] {
  override def plus[A](a: IO[E, A], b: => IO[E, A]): IO[E, A] = a.catchAll(_ => b)
}

// annoyingly, we just need M.zero
private class IOMonadPlus[E](implicit M: Monoid[E]) extends IOMonadError[E] with IOPlus[E] with MonadPlus[IO[E, ?]] {
  override def empty[A]: IO[E, A] = raiseError(M.zero)
}

private trait IOBifunctor extends Bifunctor[IO] {
  override def bimap[A, B, C, D](fab: IO[A, B])(f: A => C, g: B => D): IO[C, D] =
    IO.absolve(fab.attempt.map(_.bimap(f, g)))
}

private class IOParApplicative[E] extends Applicative[IO.Par[E, ?]] {
  override def point[A](a: => A): IO.Par[E, A] = Tag(IO.point(a))
  override def ap[A, B](fa: => IO.Par[E, A])(f: => IO.Par[E, A => B]): IO.Par[E, B] = {
    lazy val fa0: IO[E, A] = Tag.unwrap(fa)
    Tag(Tag.unwrap(f).flatMap(x => fa0.map(x)))
  }

  override def map[A, B](fa: IO.Par[E, A])(f: A => B): IO.Par[E, B] =
    Tag(Tag.unwrap(fa).map(f))

  override def apply2[A, B, C](
    fa: => IO.Par[E, A],
    fb: => IO.Par[E, B]
  )(f: (A, B) => C): IO.Par[E, C] =
    Tag(Tag.unwrap(fa).par(Tag.unwrap(fb)).map(f.tupled))
}
