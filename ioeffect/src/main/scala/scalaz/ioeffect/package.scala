// Copyright (C) 2018 John A. De Goes. All rights reserved.

package scalaz

import scala.concurrent.duration.Duration

package object ioeffect {

  implicit class IOVoidSyntax[A](val io: IO[Void, A]) extends AnyRef {
    def apply[E]: IO[E, A] = io.asInstanceOf[IO[E, A]]
  }

  type Task[A] = IO[Throwable, A]
  object Task {
    final def now[A](effect: A): Task[A]                                              = IO.now(effect)
    final def point[A](effect: => A): Task[A]                                         = IO.point(effect)
    final def sync[A](effect: => A): Task[A]                                          = IO.sync(effect)
    final def async[A](register: (ExitResult[Throwable, A] => Unit) => Unit): Task[A] = IO.async(register)

    final def fail[A](error: Throwable): Task[A] = IO.fail(error)

    final def unit: Task[Unit]                      = IO.unit
    final def sleep(duration: Duration): Task[Unit] = IO.sleep(duration)
  }

  type Unexceptional[A] = IO[Void, A]

  type Void = Void.Void // required at this level for working Void

  val Void: VoidModule = VoidImpl

  type Canceler     = Throwable => Unit
  type PureCanceler = Throwable => IO[Void, Unit]
}
