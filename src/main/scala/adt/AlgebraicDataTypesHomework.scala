package main.scala.adt

import main.scala.adt.AlgebraicDataTypesHomework.HandType.{Omaha, Texas}

object AlgebraicDataTypesHomework extends App{
    // Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
    // task you completed to join the bootcamp. Use your best judgement about particular data types to include
    // in the solution, you can model concepts like:
    //
    // 1. Suit
    // 2. Rank
    // 3. Card
    // 4. Hand (Texas or Omaha)
    // 5. Board
    // 6. Poker Combination (High Card, Pair, etc.)
    // 7. Test Case (Board & Hands to rank)
    // 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)

    sealed trait Suit
    object Suit {
        final case object Spades extends Suit
        final case object Hearts extends Suit
        final case object Clubs extends Suit
        final case object Diamonds extends Suit
    }

    sealed trait Rank {def name: String; def weight: Int}
    object Rank {
        final case object A extends Rank {val name = "A"; val weight = 14}
        final case object K extends Rank {val name = "K"; val weight = 13}
        final case object Q extends Rank {val name = "Q"; val weight = 12}
        final case object J extends Rank {val name = "J"; val weight = 11}
        final case object T extends Rank {val name = "T"; val weight = 10}
        final case object Nine extends Rank {val name = "9"; val weight = 9}
        final case object Eight extends Rank {val name = "8"; val weight = 8}
        final case object Seven extends Rank {val name = "7"; val weight = 7}
        final case object Six extends Rank {val name = "6"; val weight = 6}
        final case object Five extends Rank {val name = "5"; val weight = 5}
        final case object Four extends Rank {val name = "4"; val weight = 4}
        final case object Three extends Rank {val name = "3"; val weight = 3}
        final case object Two extends Rank {val name = "2"; val weight = 2}

        def create (name : String): Option[Rank] = name match {
            case "A" => Some(A)
            case "K" => Some(K)
            case "Q" => Some(Q)
            case "J" => Some(J)
            case "T" => Some(T)
            case "9" => Some(Nine)
            case "8" => Some(Eight)
            case "7" => Some(Seven)
            case "6" => Some(Six)
            case "5" => Some(Five)
            case "4" => Some(Four)
            case "3" => Some(Three)
            case "2" => Some(Two)
            case _ => None
        }
    }

    final case class Card (rank: Rank, suit: Suit)

    sealed trait HandType
    object HandType {
        final case object Texas extends HandType
        final case object Omaha extends HandType
    }

    final case class Hand (cards: Set[Card])
    object Hand {
        def create (cards: Set[Card], handType: HandType): Option[Hand] = handType match {
            case Texas => cards match {
                case _ if cards.size != 2 => None
                case _ => Some(Hand(cards))
            }

            case Omaha => cards match {
                case _ if cards.size != 4 => None
                case _ => Some(Hand(cards))
            }
        }
    }

    final case class Board (cards: Set[Card])
    object Board {
        def create (cards: Set[Card]): Option[Set[Card]] = cards match {
            case _ if cards.size != 5 => None
            case _ => Some(cards)
        }
    }

    sealed trait PokerCombination
    object PokerCombination {
        final case class HighCard (board: Board, hand: Hand) extends PokerCombination
        final case class Pair (board: Board, hand: Hand) extends PokerCombination
        final case class TwoPairs (board: Board, hand: Hand) extends PokerCombination
        final case class ThreeOfKind (board: Board, hand: Hand) extends PokerCombination
        final case class FullHouse (board: Board, hand: Hand) extends PokerCombination
        final case class Straight (board: Board, hand: Hand) extends PokerCombination
        final case class Flush (board: Board, hand: Hand) extends PokerCombination
        final case class FourOfKind (board: Board, hand: Hand) extends PokerCombination
        final case class StraightFlush (board: Board, hand: Hand) extends PokerCombination
    }

    final case class TestCase (board: Board, hand: Hand)

    final case class Result (sortedHand: List[Hand])

    final case class TestResult (testCase: TestCase, result: Result)
}
