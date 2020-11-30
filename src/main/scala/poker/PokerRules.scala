package poker

import PokerDomain._
import Suite._

sealed trait PokerRule {
  def name: String

  @deprecated
  def findWinner(player1Hand: Hand, player2Hand: Hand): Winner

  def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    findWinner(player1Hand, player2Hand)
      .map { player => (player, name, Set())}
  }

  protected def handValues(hand: Hand): Seq[Int] = hand.map {_._1}

  protected def handSuites(hand: Hand): Seq[Suite] = hand.map {_._2}

  protected def handHasCardsWithRepeatedValuesNTimes(hand: Hand, times: Int): Option[Seq[Card]] = {
    hand
      .groupBy { _._1 }
      .find { _._2.length == times }
      .map { _._2 }
  }

  protected def ruleWinnerWithCards(cards: (Option[Seq[Card]], Option[Seq[Card]])): Option[(Int, String, Set[Card])] = {
    cards match {
      case (Some(player1Cards), None) => Some(1, name, player1Cards.toSet)
      case (None, Some(player2Cards)) => Some(2, name, player2Cards.toSet)
      case (Some(player1Cards), Some(player2Cards)) if player1Cards.head._1 > player2Cards.head._1 => Some(1, name, player1Cards.toSet)
      case (Some(player1Cards), Some(player2Cards)) if player1Cards.head._1 < player2Cards.head._1 => Some(2, name, player2Cards.toSet)
      case _ => None
    }
  }

  protected def handHasCardCombinationsWithRepeatedValuesNTimes(hand: Hand, firstTimes: Int, secondTimes:Int): Option[(Seq[Card], Seq[Card])] = {
    val groupingsByValue = hand.groupBy { _._1 }

    val firstSetOfCards: Option[(Int, Seq[(Int, Suite)])] = groupingsByValue.find { _._2.length == firstTimes }
    val updatedGroupings = groupingsByValue.filterNot { group => firstSetOfCards.map { _._1 }.contains(group._1) }
    val secondSetOfCards  = updatedGroupings.find { _._2.length == secondTimes }

    (firstSetOfCards.map {_._2}, secondSetOfCards.map { _._2 }) match {
      case (Some(cards1), Some(cards2)) => Some((cards1, cards2))
      case _ => None
    }
  }

  protected def ruleWinnerWithCombinationCards(cards: (Option[(Seq[Card], Seq[Card])], Option[(Seq[Card], Seq[Card])])): Option[(Int, String, Set[Card])] = {
    cards match {
      case (Some((p1c1, p1c2)), None) => Some(1, name, (p1c1 ++ p1c2).toSet)
      case (None, Some((p2c1, p2c2))) => Some(2, name, (p2c1 ++ p2c2).toSet)
      case (Some((p1c1, p1c2)), Some((p2c1, _))) if (p1c1.head._1 > p2c1.head._1) => Some(1, name, (p1c1 ++ p1c2).toSet)
      case (Some((p1c1, _)), Some((p2c1, p2c2))) if (p1c1.head._1 < p2c1.head._1) => Some(2, name, (p2c1 ++ p2c2).toSet)
      case (Some((p1c1, p1c2)), Some((p2c1, p2c2))) if (p1c1.head._1 == p2c1.head._1 && p1c2.head._1 > p2c2.head._1) => Some(1, name, (p1c1 ++ p1c2).toSet)
      case (Some((p1c1, p1c2)), Some((p2c1, p2c2))) if (p1c1.head._1 == p2c1.head._1 && p1c2.head._1 < p2c2.head._1) => Some(2, name, (p2c1 ++ p2c2).toSet)
      case _ => None
    }
  }

  protected def handHasConsecutiveCardValues(hand: Hand): Option[Hand] = {
    val highestInConsecutive: Int = handValues(hand)
      .sorted
      .fold(-1)((acc: Int, value: Int) => acc match {
        case -1 => value
        case _ if value - acc == 1 => value
        case _ => 0
      })
    if (highestInConsecutive > 0) Some(hand) else None
  }

  @deprecated
  protected def handHasValueRepeatedNTimes(hand: Hand, times: Int): Option[Int] = {
    val values = handValues(hand)
    values
      .map { cardValue: Int => cardValue -> values.count(y => cardValue == y) }
      .find {_._2 == times}
      .map {_._1}
  }

  @deprecated
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

  @deprecated
  protected def handHasConsecutiveValues(hand: Hand): Option[Int] = {
    val highestInConsecutive: Int = handValues(hand)
      .sorted
      .fold(-1)((acc: Int, value: Int) => acc match {
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

  @deprecated
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    Range.inclusive(0, 4)
      .map { index => (Some(Seq(player1Hand(index))), Some(Seq(player2Hand(index)))) }
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCards((
      handHasCardsWithRepeatedValuesNTimes(player1Hand, 2),
      handHasCardsWithRepeatedValuesNTimes(player2Hand, 2)
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCombinationCards((
      handHasCardCombinationsWithRepeatedValuesNTimes(player1Hand, 2,2),
      handHasCardCombinationsWithRepeatedValuesNTimes(player2Hand, 2,2)
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCards((
      handHasCardsWithRepeatedValuesNTimes(player1Hand, 3),
      handHasCardsWithRepeatedValuesNTimes(player2Hand, 3)
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCards((
      handHasConsecutiveCardValues(player1Hand),
      handHasConsecutiveCardValues(player2Hand)
    ))
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

  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCards((
      handHasFlush(player1Hand),
      handHasFlush(player2Hand)
    ))
  }
  private def highestCardWithFlush(hand: Hand): Option[Int] = {
    if (hand.map {_._2}.distinct.length != 1) None
    else hand.map {_._1}.headOption
  }
  private def handHasFlush(hand: Hand): Option[Hand] = {
    if (hand.map {_._2}.distinct.length != 1) None
    else Some(hand)
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCombinationCards((
      handHasCardCombinationsWithRepeatedValuesNTimes(player1Hand, 3,2),
      handHasCardCombinationsWithRepeatedValuesNTimes(player2Hand, 3,2)
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
  override def findWinnerAndCards(player1Hand: Hand, player2Hand: Hand): Option[(Int, String, Set[Card])] = {
    ruleWinnerWithCards((
      handHasCardsWithRepeatedValuesNTimes(player1Hand, 4),
      handHasCardsWithRepeatedValuesNTimes(player2Hand, 4)
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
