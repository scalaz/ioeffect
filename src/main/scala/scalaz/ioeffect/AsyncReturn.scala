// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

/**
 * The `AsyncReturn` class describes the return value of an asynchronous effect
 * that is imported into an `IO` value.
 *
 * Asynchronous effects can return `later`, which represents an uninterruptible
 * asynchronous action, `now` which represents a synchronously computed value,
 * or `maybeLater`, which represents an interruptible asynchronous action.
 */
sealed abstract class AsyncReturn[A] { self =>
  import AsyncReturn._

  final def fold[Z](later: =>Z, now: A => Z, maybeLater: Canceler => Z): Z =
    self match {
      case Now(v)        => now(v)
      case MaybeLater(c) => maybeLater(c)
      case Later()       => later
    }
}
object AsyncReturn {
  type Canceler = Throwable => Unit

  final case class Now[A](value: A)                  extends AsyncReturn[A]
  final case class MaybeLater[A](canceler: Canceler) extends AsyncReturn[A]
  sealed abstract case class Later[A] private ()     extends AsyncReturn[A]

  object Later {
    // avoids object alloc, whilst allowing exhaustivity checking
    // the Later_ subclass is for scala/bug#10822
    // (it makes the pattern matcher think that `Later()` can match `AsyncReturn[A]`)
    private[this] final class Later_[+A /* scalafix:ok */ ] extends Later[A]
    private[this] val value: Later[Nothing] = new Later_[Nothing]
    def apply[A](): AsyncReturn[A]          = value.asInstanceOf[AsyncReturn[A]]
  }

  /**
   * Constructs an `AsyncReturn` that represents an uninterruptible asynchronous
   * action. The action should invoke the callback passed to the handler when
   * the value is available or the action has failed.
   *
   * See `IO.async0` for more information.
   */
  final def later[A]: AsyncReturn[A] = Later()

  /**
   * Constructs an `AsyncReturn` that represents a synchronous return. The
   * handler should never invoke the callback.
   *
   * See `IO.async0` for more information.
   */
  final def now[A](a: A): AsyncReturn[A] = Now(a)

  /**
   * Constructs an `AsyncReturn` that represents an interruptible asynchronous
   * action. The action should invoke the callback passed to the handler when
   * the value is available or the action has failed.
   *
   * The specified canceler, which must be idempotent, should attempt to cancel
   * the asynchronous action to avoid wasting resources for an action whose
   * results are no longer needed because the fiber computing them has been
   * terminated.
   */
  final def maybeLater[A](canceler: Canceler): AsyncReturn[A] =
    MaybeLater(canceler)
}
