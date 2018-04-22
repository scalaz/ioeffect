package scalaz.ioeffect

import java.io.{ByteArrayOutputStream, PrintStream}

import cats.effect.laws.discipline.ConcurrentEffectTests
import org.typelevel.discipline.scalatest.Discipline
import cats.effect.laws.util.{TestContext, TestInstances}
import org.typelevel.discipline.Laws
import org.scalatest.prop.Checkers
import org.scalatest.{FunSuite, Matchers, Tag}
import scalaz.ioeffect.catscompat._
import cats.implicits._
import scala.util.control.NonFatal

class IOCatsLawsTest
    extends FunSuite
    with Matchers
    with Checkers
    with Discipline
    with IOScalaCheckInstances {


  /**
    * Silences `System.err`, only printing the output in case exceptions are
    * thrown by the executed `thunk`.
    */
  def silenceSystemErr[A](thunk: => A): A = synchronized {
    // Silencing System.err
    val oldErr = System.err
    val outStream = new ByteArrayOutputStream()
    val fakeErr = new PrintStream(outStream)
    System.setErr(fakeErr)
    try {
      val result = thunk
      System.setErr(oldErr)
      result
    } catch {
      case NonFatal(e) =>
        System.setErr(oldErr)
        // In case of errors, print whatever was caught
        fakeErr.close()
        val out = outStream.toString("utf-8")
        if (out.nonEmpty) oldErr.println(out)
        throw e
    }
  }

  def checkAllAsync(name: String, f: TestContext => Laws#RuleSet) {
    val context = TestContext()
    val ruleSet = f(context)

    for ((id, prop) ← ruleSet.all.properties)
      test(name + "." + id) {
        silenceSystemErr(check(prop))
      }
  }

  checkAllAsync("ConcurrentEffect[Task]", implicit e => ConcurrentEffectTests[Task].concurrentEffect[Int, Int, Int])
}
