package scalaz.ioeffect

import cats.effect.laws.ConcurrentEffectLaws
import cats.Eq
import org.scalacheck.Arbitrary

class IOScalaCheckInstances {

  implicit lazy val equalityThrowable = new Eq[Throwable] {
    override def eqv(x: Throwable, y: Throwable): Boolean = {
      x.getClass == y.getClass && x.getMessage == x.getMessage
    }
  }

  implicit def catsEQ[A](implicit E: Eq[A]): Eq[Task[A]] = ???


  implicit def arbitraryIO[A](implicit A: Arbitrary[A]): Arbitrary[Task[A]] = {
    import Arbitrary._
    def genPure =
      arbitrary[A].map(IO.now)

    def genSyncThrowable: Arbitrary[Task[A]] = ???


    ???
  }

}
