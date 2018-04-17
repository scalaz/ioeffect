package scalaz
package ioeffect

import com.github.ghik.silencer.silent

trait VoidModule {
  type Void

  def absurd[A](v: Void): A

  def unsafeVoid: Void
}

trait VoidFunctions {
  @inline final def absurd[A](v: Void): A = Void.absurd[A](v)
}

trait VoidSyntax {
  implicit class Ops(v: Void) {
    def absurd[A]: A = Void.absurd(v)
  }
}

// NOTE: this is some next level black compiler magic
// but without this object syntax doesn't resolve...
object VoidModule extends VoidSyntax

@silent
private[ioeffect] object VoidImpl extends VoidModule with VoidSyntax {
  type Void = Nothing

  def absurd[A](v: Void): A = v

  private[ioeffect] final class UnsafeVoid extends RuntimeException

  def unsafeVoid: Void = throw new UnsafeVoid

}
