package main.scala.error_handling

import cats.data.ValidatedNec
import cats.syntax.all._

object ErrorHandlingHomeWork extends App {

    // Homework. Place the solution under `error_handling` package in your homework repository.
    //
    // 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
    // 2. Add `ValidationError` cases (at least 5, may be more).
    // 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

    case class PaymentCard(name: String, number: String, expirationDate: String, securityCode: String)

    sealed trait ValidationError

    object ValidationError {

        final case object SecurityCodeIsNotNumeric extends ValidationError {
            override def toString: String = "Security code must be a number"
        }

        final case object SecurityCodeWrongLength extends ValidationError {
            override def toString: String = "Security code must be a 3 digit number"
        }

        final case object NameHasSpecialCharacters extends ValidationError {
            override def toString: String = "Name cannot contain special characters"
        }

        final case object NameWrongLength extends ValidationError {
            override def toString: String = "Name must be between 3 and 30 characters"
        }

        final case object ExpirationDateWrongFormat extends ValidationError {
            override def toString: String = "Expiration date must satisfy MM/YY or MM/YYYY format"
        }

        final case object CardNumberIsNotNumeric extends ValidationError {
            override def toString: String = "Card number must be a number"
        }

        final case object CardNumberWrongLength extends ValidationError {
            override def toString: String = "Card number must be a 16 digit long"
        }

    }

    object PaymentCardValidator {
        import ValidationError._

        type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

        def validate(
                        name: String,
                        number: String,
                        expirationDate: String,
                        securityCode: String,
                    ): AllErrorsOr[PaymentCard] = (
            validateName(name),
            validateNumber(number),
            validateExpirationDate(expirationDate),
            validateSecurityCode(securityCode)
            ).mapN(PaymentCard)

        private def validateName(name: String): AllErrorsOr[String] = {
            def validateNameContents: AllErrorsOr[String] =
                if (name.matches("\\b([A-ZÀ-ÿ][-,a-z. ']+[ ]*)+")) name.validNec
                else NameHasSpecialCharacters.invalidNec

            def validateNameLength: AllErrorsOr[String] =
                if (name.length > 3 && name.length < 30) name.validNec
                else NameWrongLength.invalidNec

            validateNameLength.productR(validateNameContents)
        }

        private def validateNumber(number: String): AllErrorsOr[String] = {
            def validateNumberIsNumeric: AllErrorsOr[String] =
                if (number.matches("^(0|[1-9][0-9]*)$")) number.validNec
                else CardNumberIsNotNumeric.invalidNec

            def validateNumberLength: AllErrorsOr[String] =
                if (number.length == 16) number.validNec
                else CardNumberWrongLength.invalidNec

            validateNumberLength.productR(validateNumberIsNumeric)
        }

        private def validateExpirationDate(date: String): AllErrorsOr[String] = {
            if (date.matches("^(0[1-9]|1[0-2])\\/?([0-9]{4}|[0-9]{2})$")) date.validNec
            else ExpirationDateWrongFormat.invalidNec
        }

        private def validateSecurityCode(code: String): AllErrorsOr[String] = {
            def validateSecurityCodeIsNumeric: AllErrorsOr[String] =
                if (code.matches("^(0|[1-9][0-9]*)$")) code.validNec
                else SecurityCodeIsNotNumeric.invalidNec

            def validateSecurityCodeLength: AllErrorsOr[String] =
                if (code.length == 3) code.validNec
                else SecurityCodeWrongLength.invalidNec

            validateSecurityCodeLength.productR(validateSecurityCodeIsNumeric)
        }
    }
}
