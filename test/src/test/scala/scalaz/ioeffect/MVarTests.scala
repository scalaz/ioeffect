package scalaz.ioeffect

import org.specs2.Specification
import org.specs2.concurrent.ExecutionEnv
import org.specs2.specification.AroundTimeout
import org.specs2.specification.core.SpecStructure

import scala.concurrent.ExecutionContext
import scalaz._

class MVarTests(implicit ee: ExecutionEnv)
    extends Specification
    with AroundTimeout {
  def getEc = ExecutionContext.Implicits.global

  def is: SpecStructure =
    s2"""
         MVar Specification
           have deterministic single take/put behavior            $singleMVarEmptyTest
           have deterministic single take/put behavior on empty   $singleMVarTest
           succeed on peek non empty mvar                         $peekOnNonEmpty
           fail on peek on empty mvar                             $peekOnEmpty
           succeed on tryput on an empty mvar                     $tryPutOnEmpty
           fail on tryput on a nonempty mvar                      $tryPutOnEmpty
           succeed on tryTake on a non empty mvar                 $tryTakeOnNonEmpty
           fail on tryTake on empty mvar                          $tryTakeOnEmpty
           have deterministic sequential take/put behaviour       $sequentialMVarTest
      """

  def singleMVarEmptyTest = {
    val ioAction = for {
      mvar <- MVar.newEmptyMVar[Int](getEc)
      _    <- mvar.put(1)
      a    <- mvar.read
      b    <- mvar.take
    } yield a -> b

    ioAction.unsafePerformIO().must_==(1 -> 1)
  }

  def singleMVarTest = {
    val ioAction = for {
      mvar <- MVar.newMVar(1)(getEc)
      a    <- mvar.read
      b    <- mvar.take
    } yield a -> b

    ioAction.unsafePerformIO().must_==(1 -> 1)
  }

  def tryPutOnEmpty = {
    val ioAction = for {
      mvar <- MVar.newEmptyMVar[Int](getEc)
      put1 <- mvar.tryPut(1)
      put2 <- mvar.tryPut(2)
    } yield put1 && !put2

    ioAction.unsafePerformIO().must_==(true)
  }

  def tryPutOnNonEmpty = {
    val ioAction = for {
      mvar <- MVar.newMVar[Int](1)(getEc)
      res  <- mvar.tryPut(2)
    } yield res

    ioAction.unsafePerformIO().must_==(false)
  }

  def tryTakeOnEmpty = {
    val ioAction = for {
      mvar <- MVar.newEmptyMVar[Int](getEc)
      get1 <- mvar.tryTake
    } yield get1

    ioAction.unsafePerformIO().must_==(Maybe.empty)
  }

  def tryTakeOnNonEmpty = {
    val ioAction = for {
      mvar <- MVar.newMVar[Int](1)(getEc)
      get1 <- mvar.tryTake
      get2 <- mvar.tryTake
    } yield get1 -> get2

    ioAction.unsafePerformIO().must_==(Maybe.just(1) -> Maybe.empty)
  }

  def peekOnEmpty = {
    val ioAction = for {
      mvar <- MVar.newEmptyMVar[Int](getEc)
      get2 <- mvar.peek
    } yield get2

    ioAction.unsafePerformIO().must_==(Maybe.empty)
  }

  def peekOnNonEmpty = {
    val ioAction = for {
      mvar <- MVar.newMVar[Int](1)(getEc)
      get2 <- mvar.peek
    } yield get2

    ioAction.unsafePerformIO().must_==(Maybe.just(1))
  }

  def sequentialMVarTest = {

    val ioAction = for {
      mvar <- MVar.newEmptyMVar[Int](getEc)
      _    <- mvar.put(1)
      _    <- mvar.put(2).fork
      a    <- mvar.take
      b    <- mvar.take
    } yield (a -> b)

    ioAction.unsafePerformIO().must_==(1 -> 2)
  }

}
