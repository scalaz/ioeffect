// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

import scala.concurrent.duration._

import org.specs2.concurrent.ExecutionEnv
import org.specs2.Specification
import org.specs2.specification.AroundTimeout
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.SpecStructure

import com.github.ghik.silencer.silent

class RTSSpec(implicit ee: ExecutionEnv)
    extends Specification
    with AroundTimeout
    with RTS {

  override val defaultHandler: Throwable => IO[Unit] = t => IO.unit

  def is: SpecStructure =
    s2"""
  RTS synchronous correctness
    evaluation of point                     $testPoint
    point must be lazy                      $testPointIsLazy
    now must be eager                       $testNowIsEager
    suspend must be lazy                    $testSuspendIsLazy
    suspend must be evaluatable             $testSuspendIsEvaluatable
    point, bind, map                        $testSyncEvalLoop
    sync effect                             $testEvalOfSyncEffect
    deep effects                            $testEvalOfDeepSyncEffect

  RTS failure
    error in sync effect                    $testEvalOfAttemptOfSyncEffectError
    attempt . fail                          $testEvalOfAttemptOfFail
    deep attempt sync effect error          $testAttemptOfDeepSyncEffectError
    deep attempt fail error                 $testAttemptOfDeepFailError
    uncaught fail                           $testEvalOfUncaughtFail
    uncaught sync effect error              $testEvalOfUncaughtThrownSyncEffect
    deep uncaught sync effect error         $testEvalOfDeepUncaughtThrownSyncEffect
    deep uncaught fail                      $testEvalOfDeepUncaughtFail

  RTS bracket
    fail ensuring                           $testEvalOfFailEnsuring
    fail on error                           $testEvalOfFailOnError
    finalizer errors not caught             $testErrorInFinalizerCannotBeCaught
    finalizer errors reported               ${upTo(1.second)(
      testErrorInFinalizerIsReported
    )}
    bracket result is usage result          $testBracketResultIsUsageResult
    error in just acquisition               $testBracketErrorInAcquisition
    error in just release                   $testBracketErrorInRelease
    error in just usage                     $testBracketErrorInUsage
    rethrown caught error in acquisition    $testBracketRethrownCaughtErrorInAcquisition
    rethrown caught error in release        $testBracketRethrownCaughtErrorInRelease
    rethrown caught error in usage          $testBracketRethrownCaughtErrorInUsage
    test eval of async fail                 $testEvalOfAsyncAttemptOfFail

  RTS synchronous stack safety
    deep map of point                       $testDeepMapOfPoint
    deep map of now                         $testDeepMapOfNow
    deep map of sync effect                 $testDeepMapOfSyncEffectIsStackSafe
    deep attempt                            $testDeepAttemptIsStackSafe

  RTS asynchronous stack safety
    deep bind of async chain                $testDeepBindOfAsyncChainIsStackSafe

  RTS asynchronous correctness
    simple async must return                $testAsyncEffectReturns
    sleep 0 must return                     ${upTo(1.second)(
      testSleepZeroReturns
    )}

  RTS concurrency correctness
    shallow fork/join identity              $testForkJoinIsId
    deep fork/join identity                 $testDeepForkJoinIsId
    interrupt of never                      ${upTo(1.second)(
      testNeverIsInterruptible
    )}
    race of value & never                   ${upTo(1.second)(
      testRaceOfValueNever
    )}
  """

  def testPoint: MatchResult[Int] =
    unsafePerformIO(IO.point(1)).must_===(1)

  def testPointIsLazy: MatchResult[IO[Nothing]] =
    IO.point(throw new Error("Not lazy")).must(not(throwA[Throwable]))

  @silent
  def testNowIsEager: MatchResult[IO[Nothing]] =
    (IO.now(throw new Error("Eager"))).must(throwA[Error])

  def testSuspendIsLazy: MatchResult[IO[Nothing]] =
    IO.suspend(throw new Error("Eager")).must(not(throwA[Throwable]))

  def testSuspendIsEvaluatable: MatchResult[Int] =
    unsafePerformIO(IO.suspend(IO.point(42))).must_===(42)

  def testSyncEvalLoop: MatchResult[BigInt] = {
    def fibIo(n: Int): IO[BigInt] =
      if (n <= 1) IO.point(n)
      else
        for {
          a <- fibIo(n - 1)
          b <- fibIo(n - 2)
        } yield a + b

    unsafePerformIO(fibIo(10)).must_===(fib(10))
  }

  def testEvalOfSyncEffect: MatchResult[Int] = {
    def sumIo(n: Int): IO[Int] =
      if (n <= 0) IO.sync(0)
      else IO.sync(n).flatMap(b => sumIo(n - 1).map(a => a + b))

    unsafePerformIO(sumIo(1000)).must_===(sum(1000))
  }

  def testEvalOfAttemptOfSyncEffectError: MatchResult[\/[Throwable, Nothing]] =
    unsafePerformIO(IO.sync(throw ExampleError).attempt)
      .must_===(-\/(ExampleError))

  def testEvalOfAttemptOfFail: MatchResult[\/[Throwable, Int]] = {
    unsafePerformIO(IO.fail[Int](ExampleError).attempt)
      .must_===(-\/(ExampleError))

    unsafePerformIO(IO.suspend(IO.suspend(IO.fail[Int](ExampleError)).attempt))
      .must_===(-\/(ExampleError))
  }

  def testAttemptOfDeepSyncEffectError: MatchResult[\/[Throwable, Unit]] =
    unsafePerformIO(deepErrorEffect(100).attempt).must_===(-\/(ExampleError))

  def testAttemptOfDeepFailError: MatchResult[\/[Throwable, Unit]] =
    unsafePerformIO(deepErrorFail(100).attempt).must_===(-\/(ExampleError))

  def testEvalOfUncaughtFail: MatchResult[Int] =
    unsafePerformIO(IO.fail[Int](ExampleError)).must(throwA(ExampleError))

  def testEvalOfUncaughtThrownSyncEffect: MatchResult[Int] =
    unsafePerformIO(IO.sync[Int](throw ExampleError)).must(throwA(ExampleError))

  def testEvalOfDeepUncaughtThrownSyncEffect: MatchResult[Unit] =
    unsafePerformIO(deepErrorEffect(100)).must(throwA(ExampleError))

  def testEvalOfDeepUncaughtFail: MatchResult[Unit] =
    unsafePerformIO(deepErrorEffect(100)).must(throwA(ExampleError))

  def testEvalOfFailEnsuring: MatchResult[Boolean] = {
    var finalized = false

    unsafePerformIO(
      IO.fail[Unit](ExampleError)
        .ensuring(IO.sync[Unit] { finalized = true; () })
    ).must(
      throwA(
        ExampleError
      )
    )
    finalized.must_===(true)
  }

  def testEvalOfFailOnError: MatchResult[Boolean] = {
    var finalized = false

    unsafePerformIO(
      IO.fail[Unit](ExampleError)
        .onError(_ => IO.sync[Unit] { finalized = true; () })
    ).must(
      throwA(
        ExampleError
      )
    )

    finalized.must_===(true)
  }

  def testErrorInFinalizerCannotBeCaught: MatchResult[Int] = {
    val nested: IO[Int] =
      IO.fail(ExampleError)
        .ensuring(IO.fail(new Error("e2")))
        .ensuring(IO.fail(new Error("e3")))

    unsafePerformIO(nested).must(throwA(ExampleError))
  }

  def testErrorInFinalizerIsReported: MatchResult[Throwable] = {
    var reported: Throwable = null

    unsafePerformIO {
      IO.point(42)
        .ensuring(IO.fail(ExampleError))
        .fork0(e => IO.sync[Unit] { reported = e; () })
    }

    // FIXME: Is this an issue with thread synchronization?
    while (reported == null) Thread.`yield`()

    reported.must_===(ExampleError)
  }

  def testBracketResultIsUsageResult: MatchResult[Int] =
    unsafePerformIO(IO.unit.bracket_(IO.unit)(IO.point(42))).must_===(42)

  def testBracketErrorInAcquisition: MatchResult[Unit] =
    unsafePerformIO(IO.fail[Unit](ExampleError).bracket_(IO.unit)(IO.unit))
      .must(throwA(ExampleError))

  def testBracketErrorInRelease: MatchResult[Unit] =
    unsafePerformIO(IO.unit.bracket_(IO.fail[Unit](ExampleError))(IO.unit))
      .must(throwA(ExampleError))

  def testBracketErrorInUsage: MatchResult[Unit] =
    unsafePerformIO(IO.unit.bracket_(IO.unit)(IO.fail[Unit](ExampleError)))
      .must(throwA(ExampleError))

  def testBracketRethrownCaughtErrorInAcquisition: MatchResult[Unit] = {
    lazy val actual = unsafePerformIO(
      IO.absolve(IO.fail[Unit](ExampleError).bracket_(IO.unit)(IO.unit).attempt)
    )

    actual.must(throwA(ExampleError))
  }

  def testBracketRethrownCaughtErrorInRelease: MatchResult[Unit] = {
    lazy val actual = unsafePerformIO(
      IO.absolve(IO.unit.bracket_(IO.fail[Unit](ExampleError))(IO.unit).attempt)
    )

    actual.must(throwA(ExampleError))
  }

  def testBracketRethrownCaughtErrorInUsage: MatchResult[Unit] = {
    lazy val actual = unsafePerformIO(
      IO.absolve(IO.unit.bracket_(IO.unit)(IO.fail[Unit](ExampleError)).attempt)
    )

    actual.must(throwA(ExampleError))
  }

  def testEvalOfAsyncAttemptOfFail: MatchResult[Unit] = {
    val io1 = IO.unit.bracket_(AsyncUnit)(asyncExampleError[Unit])
    val io2 = AsyncUnit.bracket_(IO.unit)(asyncExampleError[Unit])

    unsafePerformIO(io1).must(throwA(ExampleError))
    unsafePerformIO(io2).must(throwA(ExampleError))
    unsafePerformIO(IO.absolve(io1.attempt)).must(throwA(ExampleError))
    unsafePerformIO(IO.absolve(io2.attempt)).must(throwA(ExampleError))
  }

  def testEvalOfDeepSyncEffect: MatchResult[Int] = {
    def incLeft(n: Int, ref: IORef[Int]): IO[Int] =
      if (n <= 0) ref.read
      else incLeft(n - 1, ref) <* ref.modify(_ + 1)

    def incRight(n: Int, ref: IORef[Int]): IO[Int] =
      if (n <= 0) ref.read
      else ref.modify(_ + 1) *> incRight(n - 1, ref)

    unsafePerformIO(for {
      ref <- IORef(0)
      v   <- incLeft(100, ref)
    } yield v).must_===(100)

    unsafePerformIO(for {
      ref <- IORef(0)
      v   <- incRight(1000, ref)
    } yield v).must_===(1000)
  }

  def testDeepMapOfPoint: MatchResult[Int] =
    unsafePerformIO(deepMapPoint(10000)).must_===(10000)

  def testDeepMapOfNow: MatchResult[Int] =
    unsafePerformIO(deepMapNow(10000)).must_===(10000)

  def testDeepMapOfSyncEffectIsStackSafe: MatchResult[Int] =
    unsafePerformIO(deepMapEffect(10000)).must_===(10000)

  def testDeepAttemptIsStackSafe: MatchResult[Unit] =
    unsafePerformIO(0.until(10000).foldLeft(IO.sync(())) { (acc, _) =>
      acc.attempt.toUnit
    }).must_===(())

  def testDeepBindOfAsyncChainIsStackSafe: MatchResult[Int] = {
    val result = 0.until(10000).foldLeft(IO.point(0)) { (acc, _) =>
      acc.flatMap(n => IO.async[Int](_(\/-(n + 1))))
    }

    unsafePerformIO(result).must_===(10000)
  }

  def testAsyncEffectReturns: MatchResult[Int] =
    unsafePerformIO(IO.async[Int](cb => cb(\/-(42)))).must_===(42)

  def testSleepZeroReturns: MatchResult[Unit] =
    unsafePerformIO(IO.sleep(1.nanoseconds)).must_===((): Unit)

  def testForkJoinIsId: MatchResult[Int] =
    unsafePerformIO(IO.point(42).fork.flatMap(_.join)).must_===(42)

  def testDeepForkJoinIsId: MatchResult[BigInt] = {
    val n = 20

    unsafePerformIO(concurrentFib(n)).must_===(fib(n))
  }

  def testNeverIsInterruptible: MatchResult[Int] = {
    val io =
      for {
        fiber <- IO.never[Int].fork
        _     <- fiber.interrupt(ExampleError)
      } yield 42

    unsafePerformIO(io).must_===(42)
  }

  def testRaceOfValueNever: Boolean =
    unsafePerformIO(IO.point(42).race(IO.never[Int])) == 42

  // Utility stuff
  val ExampleError: Error = new Error("Oh noes!")

  def asyncExampleError[A]: IO[A] = IO.async[A](_(-\/(ExampleError)))

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def deepMapPoint(n: Int): IO[Int] =
    if (n <= 0) IO.point(n) else IO.point(n - 1).map(_ + 1)

  def deepMapNow(n: Int): IO[Int] =
    if (n <= 0) IO.now(n) else IO.now(n - 1).map(_ + 1)

  def deepMapEffect(n: Int): IO[Int] =
    if (n <= 0) IO.sync(n) else IO.sync(n - 1).map(_ + 1)

  def deepErrorEffect(n: Int): IO[Unit] =
    if (n == 0) IO.sync(throw ExampleError)
    else IO.unit *> deepErrorEffect(n - 1)

  def deepErrorFail(n: Int): IO[Unit] =
    if (n == 0) IO.fail(ExampleError)
    else IO.unit *> deepErrorFail(n - 1)

  def fib(n: Int): BigInt =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  def concurrentFib(n: Int): IO[BigInt] =
    if (n <= 1) IO.point(n)
    else
      for {
        f1 <- concurrentFib(n - 1).fork
        f2 <- concurrentFib(n - 2).fork
        v1 <- f1.join
        v2 <- f2.join
      } yield v1 + v2

  val AsyncUnit: IO[Unit] = IO.async[Unit](_(\/-(())))
}
