package scalaz.ioeffect

import org.specs2.concurrent.ExecutionEnv
import org.specs2.Specification
import org.specs2.specification.AroundTimeout
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure
import scalaz._
import Scalaz._

import scala.concurrent.Future

class IOTest(implicit ee: ExecutionEnv) extends Specification with AroundTimeout with RTS {

  override def defaultHandler[E]: Throwable => IO[E, Unit] = _ => IO.unit[E]

  def is: SpecStructure =
    s2"""
    IO `fromFuture` values with `IO.now`
      values should respect Future.successful $fromFutureSpecIONowSuccessful
      values should respect Future.apply $fromFutureSpecIONowApply

    IO `fromFuture` values with `IO.point`
      values should respect Future.successful $fromFutureSpecIONowSuccessful
      values should respect Future.apply $fromFutureSpecIONowApply
      """

  def fromFutureSpecIONowSuccessful: MatchResult[Int] =
    unsafePerformIO(IO.fromFuture(IO.now(Future.successful(1)))) must_===
      unsafePerformIO(IO.now[Throwable, Int](1))

  def fromFutureSpecIONowApply: MatchResult[Int] =
    unsafePerformIO(IO.fromFuture(IO.now(Future(1)))) must_===
      unsafePerformIO(IO.now[Throwable, Int](1))

  def fromFutureSpecIOPointSuccessful: MatchResult[Int] =
    unsafePerformIO(IO.fromFuture(IO.point(Future.successful(1)))) must_===
      unsafePerformIO(IO.now[Throwable, Int](1))

  def fromFutureSpecIOPointApply: MatchResult[Int] =
    unsafePerformIO(IO.fromFuture(IO.point(Future(1)))) must_===
      unsafePerformIO(IO.now[Throwable, Int](1))

  def unsafeToFutureSpec: MatchResult[Future[Throwable \/ Int]] =
    unsafeToFuture(IO.now[Throwable, Int](1)).must_===(Future.successful(1.right[Throwable]))

}
