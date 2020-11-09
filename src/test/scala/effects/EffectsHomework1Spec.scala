package effects

import effects.EffectsHomework1._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

class EffectsHomework1Spec extends AnyFlatSpec with Matchers  {

    "IO[Int](0).map" should "return IO[Int](2)" in {
        for {
            result <- IO(2)
            action <- IO(0).map(_ + 2)
        } yield assert(result == action)
    }

    "IO[Int](0).flatMap" should "return IO[Int](2)" in {
        for {
            result <- IO(2)
            action <- IO(0).flatMap(elem => IO { elem + 2 })
        } yield assert(result == action)
    }

    "IO[Int](1) *> IO[String](1) " should "return IO[BigDecimal](1)" in {
        for {
            result <- IO("1")
            action <- IO(1) *> IO("1")
        } yield assert(result == action)
    }

    "IO[Int](1) as [String](1) " should "return IO[String](1)" in {
        for {
            result <- IO("1")
            action <- IO(1) as "1"
        } yield assert(result == action)
    }

    "IO[Int](1).void " should "return IO[Unit]" in {
        for {
            result <- IO(())
            action <- IO(1).void
        } yield assert(result === action)
    }

    "IO(new ArithmeticException).attempt " should "return IO(Left(error))" in {
        for {
            result <- IO(Left("Something went wrong"))
            action <- IO(new ArithmeticException).attempt
        } yield assert(result == action)
    }

    "IO[Int](1).attempt" should "return IO(Right(1))" in {
        for {
            result <- IO(Right(1))
            action <- IO(1).attempt
        } yield assert(result == action)
    }

    "IO[Int](1).option" should "return IO(Some(1))" in {
        for {
            result <- IO(Some(1))
            action <- IO(1).option
        } yield assert(result == action)
    }

    "IO(new ArithmeticException).option" should "return IO(None)" in {
        for {
            result <- IO(None)
            action <- IO(new ArithmeticException()).option
        } yield assert(result == action)
    }

    "IO(new ArithmeticException).handleErrorWith" should "return IO(Throwable))" in {
        for {
            result <- IO("error")
            action <- IO(new ArithmeticException()).handleErrorWith(_ => IO(new Throwable))
        } yield assert(result === action)
    }

    "IO[Int](1).redeem" should "return IO[String](1)" in {
        for {
            result <- IO("1")
            action <- IO(1).redeem(_ => "Something went wrong", _.toString)
        } yield assert(result == action)
    }

    "IO(new ArithmeticException).redeem" should "return IO[String](Something went wrong)" in {
        for {
            result <- IO("Something went wrong")
            action <- IO(new ArithmeticException()).redeem(_ => "Something went wrong", _.toString)
        } yield assert(result == action)
    }

    "IO[Int](1).redeemWith" should "return IO[String](1)" in {
        for {
            result <- IO("1")
            action <- IO(1).redeemWith(_ => IO("Something went wrong"), value => IO(value.toString))
        } yield assert(result == action)
    }

    "IO(new ArithmeticException).redeemWith" should "return IO[String](Something went wrong)" in {
        for {
            result <- IO("Something went wrong")
            action <- IO(new ArithmeticException()).redeemWith(_ => IO("Something went wrong"), value => IO(value.toString))
        } yield assert(result == action)
    }

    "IO[Int](1).unsafeRunSync" should "return 1" in {
        assert(1 == IO(1).unsafeRunSync())
    }

    "IO[Int](1).unsafeToFuture" should "return Future(1)" in {
        for {
            result <- Future(1)
            action <- IO(1).unsafeToFuture()
        } yield assert(result == action)
    }

    "IO.apply(1)" should "return IO(1)" in {
        for {
            result <- IO(1)
            action <- IO.apply(1)
        } yield assert(result == action)
    }

    "IO.suspend(IO(1))" should "return IO(1)" in {
        for {
            result <- IO(1)
            action <- IO.suspend(IO(1))
        } yield assert(result == action)
    }

    "IO.delay(1)" should "return IO(1)" in {
        for {
            result <- IO(1)
            action <- IO.delay(1)
        } yield assert(result == action)
    }

    "IO.pure(1)" should "return IO(1)" in {
        for {
            result <- IO(1)
            action <- IO.pure(1)
        } yield assert(result == action)
    }

    "IO.fromEither(Left(Throwable))" should "throw Throwable" in {
        for {
            result <- IO(new ArithmeticException())
            action <- IO.fromEither[Throwable](Left(new ArithmeticException()))
        } yield assert(result == action)
    }

    "IO.fromEither(Right(1))" should "return IO(1)" in {
        for {
            result <- IO(1)
            action <- IO.fromEither(Right(1))
        } yield assert(result == action)
    }

    "IO.fromTry(Failure(Throwable))" should "throw Throwable" in {
        for {
            result <- IO(new ArithmeticException())
            action <- IO.fromTry[Throwable](Failure(new ArithmeticException()))
        } yield assert(result == action)
    }

    "IO.fromTry(Success(1))" should "return IO(1)" in {
        for {
            result <- IO(1)
            action <- IO.fromTry(Success(1))
        } yield assert(result == action)
    }

    "IO.none[Int]" should "return IO(None)" in {
        for {
            result <- IO(None)
            action <- IO.none[Int]
        } yield assert(result == action)
    }

    "IO.raiseError(Throwable)" should "throw Throwable" in {
        for {
            result <- IO(new ArithmeticException())
            action <- IO.raiseError[Throwable](new ArithmeticException())
        } yield assert(result == action)
    }

    "IO.raiseUnless(true)" should "return IO(())" in {
        for {
            result <- IO(())
            action <- IO.raiseUnless(cond = true)(new ArithmeticException())
        } yield assert(result == action)
    }

    "IO.raiseUnless(false)" should "throw Throwable" in {
        for {
            result <- IO(())
            action <- IO.raiseUnless(cond = false)(new ArithmeticException())
        } yield assert(result == action)
    }

    "IO.raiseWhen(false)" should "return IO(())" in {
        for {
            result <- IO(())
            action <- IO.raiseWhen(cond = false)(new ArithmeticException())
        } yield assert(result == action)
    }

    "IO.raiseWhen(true)" should "throw Throwable" in {
        for {
            result <- IO(())
            action <- IO.raiseWhen(cond = true)(new ArithmeticException())
        } yield assert(result == action)
    }

    "IO.unlessA(true)" should "return IO(())" in {
        for {
            result <- IO(())
            action <- IO.unlessA(cond = true)(IO(println("Action")))
        } yield assert(result == action)
    }

    "IO.unlessA(false)" should "return IO(println(action))" in {
        for {
            result <- IO(println("action"))
            action <- IO.unlessA(cond = false)(IO(println("Action")))
        } yield assert(result == action)
    }

    "IO.whenA(true)" should "return IO(println(action))" in {
        for {
            result <- IO(println("action"))
            action <- IO.whenA(cond = true)(IO(println("Action")))
        } yield assert(result == action)
    }

    "IO.whenA(false)" should "return IO(()))" in {
        for {
            result <- IO(())
            action <- IO.whenA(cond = false)(IO(println("Action")))
        } yield assert(result == action)
    }

    "IO.unit" should "return IO(()))" in {
        for {
            result <- IO(())
            action <- IO.unit
        } yield assert(result === action)
    }

}
