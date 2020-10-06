package main.scala.basics

import scala.io.Source
import main.scala.basics.ControlStructuresHomework.Command._
import main.scala.basics.ControlStructuresHomework.Result._

object ControlStructuresHomework {

    // Homework

    // Create a command line application that reads various "commands" from the
    // stdin, evaluates them, and writes output to stdout.

    // Commands are:

    //   divide 4 5
    // which should output "4 divided by 5 is 0.8"

    //   sum 5 5 6 8.5
    // which should output "the sum of 5 5 6 8.5 is 24.5"

    //   average 4 3 8.5 4
    // which should output "the average of 4 3 8.5 4 is 4.875"

    //   min 4 -3 -17
    // which should output "the minimum of 4 -3 -17 is -17"

    //   max 4 -3 -17
    // which should output "the maximum of 4 -3 -17 is 4"

    // In case of commands that cannot be parsed or calculations that cannot be performed,
    // output a single line starting with "Error: "

    sealed trait Command

    object Command {

        final case class Divide(dividend: Double, divisor: Double) extends Command

        final case class Sum(numbers: List[Double]) extends Command

        final case class Average(numbers: List[Double]) extends Command

        final case class Min(numbers: List[Double]) extends Command

        final case class Max(numbers: List[Double]) extends Command

    }

    final case class ErrorMessage(value: String)

    sealed trait Result

    object Result {

        final case class DivideResult(dividend: Double, divisor: Double, result: Double) extends Result

        final case class SumResult(numbers: List[Double], result: Double) extends Result

        final case class AverageResult(numbers: List[Double], result: Double) extends Result

        final case class MinResult(numbers: List[Double], result: Double) extends Result

        final case class MaxResult(numbers: List[Double], result: Double) extends Result

    }

    def parseCommand(input: String): Either[ErrorMessage, Command] = {
        input.replaceAll("\\s{2,}", " ").trim.split(" ").toList match {
            case list if list.length < 2 => Left(ErrorMessage("Not enough operands"))
            case _ :: xs if xs.map(_.toDoubleOption).contains(None) => Left(ErrorMessage("Impossible to convert to numbers"))
            case x :: xs if x == "divide" => xs match {
                case twoOperands if twoOperands.length != 2 => Left(ErrorMessage("Division requires 2 operands"))
                case _ => Right(Divide(xs.head.toDouble, xs.last.toDouble))
            }
            case x :: xs if x == "sum" => Right(Sum(xs.map(elem => elem.toDouble)))
            case x :: xs if x == "average" => Right(Average(xs.map(elem => elem.toDouble)))
            case x :: xs if x == "min" => Right(Min(xs.map(elem => elem.toDouble)))
            case x :: xs if x == "max" => Right(Max(xs.map(elem => elem.toDouble)))
            case _ => Left(ErrorMessage("Unexpected error"))
        }
        // implement this method
        // Implementation hints:
        // You can use String#split, convert to List using .toList, then pattern match on:
        //   case x :: xs => ???

        // Consider how to handle extra whitespace gracefully (without errors).
    }

    // should return an error (using `Left` channel) in case of division by zero and other
    // invalid operations
    def calculate(command: Command): Either[ErrorMessage, Result] = {
        command match {
            case Divide(dividend, divisor) => (dividend, divisor) match {
                case (_, 0) => Left(ErrorMessage("Division by zero"))
                case _ => Right(DivideResult(dividend, divisor, dividend / divisor))
            }
            case Sum(numbers) => {
                Right(SumResult(numbers, numbers.sum))
            }
            case Average(numbers) => {
                Right(AverageResult(numbers, numbers.sum / numbers.size))
            }
            case Min(numbers) => {
                Right(MinResult(numbers, numbers.min))
            }
            case Max(numbers) => {
                Right(MaxResult(numbers, numbers.max))
            }
            case _ => Left(ErrorMessage("Unexpected error"))
        }
    }

    def renderResult(x: Result): String = {
        val formatter : Double => String = (number: Double) => {
          if (number == number.asInstanceOf[Long]) number.formatted("%.0f") else number.toString
        }

        x match {
            case DivideResult(dividend, divisor, result) => {
                s"${formatter(dividend)} divided by ${formatter(divisor)} is ${formatter(result)}"
            }
            case SumResult(numbers, result) => {
                s"the sum of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
            }
            case AverageResult(numbers, result) => {
                s"the average of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
            }
            case MinResult(numbers, result) => {
                s"the minimum of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
            }
            case MaxResult(numbers, result) => {
                s"the maximum of ${numbers.map(formatter).mkString(" ")} is ${formatter(result)}"
            }
        }

    }

    def process(x: String): String = {
        // the import above will enable useful operations on Either-s such as `leftMap`
        // (map over the Left channel) and `merge` (convert `Either[A, A]` into `A`),
        // but you can also avoid using them using pattern matching.

        val result = for {
            parsedCommand <- parseCommand(x)
            calculatedCommand <- calculate(parsedCommand)
        } yield calculatedCommand

        result match {
          case Left(error) => s"Error: ${error.value}"
          case Right(result) => renderResult(result)
        }

    }

    // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
    def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
