package poker

import PokerDomain._
import Suite._

sealed trait PokerRule {
  def name: String

  @deprecated
  def findWinner(player1Hand: Hand, player2Hand: Hand): Winner

  def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Seq[Card])] = {
    findWinner(player1Hand, player2Hand)
      .map { player => (player, name, Seq())}
  }

  protected def handValues(hand: Hand): Seq[Int] = hand.map {_._1}

  protected def handSuites(hand: Hand): Seq[Suite] = hand.map {_._2}

  protected def handHasValueRepeatedNTimes(hand: Hand, times: Int): Option[Int] = {
    val values = handValues(hand)
    values
      .map { cardValue: Int => cardValue -> values.count(y => cardValue == y) }
      .find {_._2 == times}
      .map {_._1}
  }

  protected def handHasCombinationsOfNTimes(hand: Hand, firstTimes: Int, secondTimes: Int): (Option[Int], Option[Int]) = {
    val values = handValues(hand)
    val valueCounts = values
      .map { x => x -> values.count(y => x == y) }
      .distinct
      .sortBy { t => (-t._2, -t._1) }

    val firstValue: Option[(Int, Int)] = valueCounts.find { _._2 == firstTimes }
    val updatedList = valueCounts.filterNot { firstValue.contains(_) }
    val secondValue: Option[(Int, Int)] = updatedList.find { _._2 == secondTimes }

    (firstValue.map {_._1}, secondValue.map {_._1}) match {
      case (Some(x), Some(y)) => (Some(x), Some(y))
      case _ => (None, None)
    }
  }

  protected def handHasConsecutiveValues(hand: Hand): Option[Int] = {
    val highestInConsecutive = handValues(hand)
      .sorted
      .fold(-1)((acc, value) => acc match {
        case -1 => value
        case _ if value - acc == 1 => value
        case _ => 0
      })
    if (highestInConsecutive > 0) Some(highestInConsecutive) else None
  }

  @deprecated
  protected def ruleWinner(values: (Option[Int], Option[Int])): Winner = {
    values match {
      case (Some(player1), Some(player2)) if player1 > player2 => Some(1)
      case (Some(player1), Some(player2)) if player1 < player2 => Some(2)
      case (Some(_), None) => Some(1)
      case (None, Some(_)) => Some(2)
      case _ => None
    }
  }

  protected def ruleWinnerWithCards(cards: (Option[Card], Option[Card])): Option[(Int, String, Seq[Card])] = {
    cards match {
      case (Some(player1Card), Some((player2, _))) if player1Card._1 > player2 => Some(1, name, Seq(player1Card))
      case (Some((player1, _)), Some(player2Card)) if player1 < player2Card._1 => Some(2, name, Seq(player2Card))
      case (Some(player1Card), None) => Some(1, name, Seq(player1Card))
      case (None, Some(player2Card)) => Some(2, name, Seq(player2Card))
      case _ => None
    }
  }

  protected def combinationWinner(values: ((Option[Int], Option[Int]), (Option[Int], Option[Int]))): Winner = {
    values match {
      case ((Some(_), Some(_)), (None, None)) => Some(1)
      case ((None, None), (Some(_), Some(_))) => Some(2)
      case ((Some(player1), Some(_)), (Some(player2), Some(_))) if player1 > player2 => Some(1)
      case ((Some(player1), Some(_)), (Some(player2), Some(_))) if player1 < player2 => Some(2)
      case ((Some(x), Some(player1)), (Some(y), Some(player2))) if x == y && player1 > player2 => Some(1)
      case ((Some(x), Some(player1)), (Some(y), Some(player2))) if x == y && player1 < player2 => Some(2)
      case _ => None
    }
  }
}

case class HighestCard() extends PokerRule {
  override def name: String = "Highest Card"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    val player1HandValues = handValues(player1Hand)
    val player2HandValues = handValues(player2Hand)
    Range.inclusive(0, 4)
      .map { index => (Some(player1HandValues(index)), Some(player2HandValues(index))) }
      .map {ruleWinner}
      .filter {_.isDefined}
      .map { case Some(player) => player }
      .headOption
  }

  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Seq[(Int, Suite)])] = {
    Range.inclusive(0, 4)
      .map { index => (Some(player1Hand(index)), Some(player2Hand(index))) }
      .map { ruleWinnerWithCards }
      .filter { _.isDefined }
      .map { case Some(player) => player }
      .headOption
  }
}

case class Pair() extends PokerRule {
  override def name: String = "Pair"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    ruleWinner((
      handHasValueRepeatedNTimes(player1Hand, 2),
      handHasValueRepeatedNTimes(player2Hand, 2)
    ))
  }
}

case class DoublePair() extends PokerRule {
  override def name: String = "Double Pair"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    combinationWinner((
      handHasCombinationsOfNTimes(player1Hand, 2, 2),
      handHasCombinationsOfNTimes(player2Hand, 2, 2)
    ))
  }
}

case class ThreeOfAKind() extends PokerRule {
  override def name: String = "Three of a kind"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    ruleWinner((
      handHasValueRepeatedNTimes(player1Hand, 3),
      handHasValueRepeatedNTimes(player2Hand, 3)
    ))
  }
}

case class Straight() extends PokerRule {
  override def name: String = "Straight"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    ruleWinner(
      handHasConsecutiveValues(player1Hand),
      handHasConsecutiveValues(player2Hand)
    )
  }
}

case class Flush() extends PokerRule {
  override def name: String = "Flush"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    ruleWinner(
      highestCardWithFlush(player1Hand),
      highestCardWithFlush(player2Hand)
    )
  }
  private def highestCardWithFlush(hand: Hand): Option[Int] = {
    if (hand.map {_._2}.distinct.length != 1) None
    else hand.map {_._1}.headOption
  }
}

case class FullHouse() extends PokerRule {
  override def name: String = "Full House"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    combinationWinner((
      handHasCombinationsOfNTimes(player1Hand, 3, 2),
      handHasCombinationsOfNTimes(player2Hand, 3, 2)
    ))
  }
}

case class FourOfAKind() extends PokerRule {
  override def name: String = "Four of a kind"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    ruleWinner((
      handHasValueRepeatedNTimes(player1Hand, 4),
      handHasValueRepeatedNTimes(player2Hand, 4)
    ))
  }
}

case class StraightFlush() extends PokerRule {
  override def name: String = "Straight Flush"
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    ruleWinner(
      playerHasConsecutiveValuesOfSameSuite(player1Hand),
      playerHasConsecutiveValuesOfSameSuite(player2Hand)
    )
  }
  private def playerHasConsecutiveValuesOfSameSuite(hand: Hand): Option[Int] = {
    if (handSuites(hand).distinct.length != 1) None
    else handHasConsecutiveValues(hand)
  }
}
