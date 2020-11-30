package poker

import PokerDomain._
import Suite._

sealed trait PokerRule {
  def name: String
  def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome

  protected def extractCombinationOfRepeatedValues(hand: Hand, times: Int): Option[Combination] = {
    hand
      .groupBy { _._1 }
      .find { _._2.length == times }
      .map { _._2 }
  }
  protected def extractMultipleCombinationsOfRepeatedValues(hand: Hand, firstTimes: Int, secondTimes:Int): Option[(Combination, Combination)] = {
    val groupingsByValue = hand.groupBy { _._1 }

    val firstSetOfCards: Option[(Int, Seq[(Int, Suite)])] = groupingsByValue.find { _._2.length == firstTimes }
    val updatedGroupings = groupingsByValue.filterNot { group => firstSetOfCards.map { _._1 }.contains(group._1) }
    val secondSetOfCards  = updatedGroupings.find { _._2.length == secondTimes }

    (firstSetOfCards.map {_._2}, secondSetOfCards.map { _._2 }) match {
      case (Some(cards1), Some(cards2)) => Some((cards1, cards2))
      case _ => None
    }
  }
  protected def handHasAllCardsSameSuite(hand: Hand): Boolean = hand.map {_._2}.distinct.length == 1
  protected def handHasConsecutiveValues(hand: Hand): Boolean = {
    hand.map {_._1}.sorted
      .fold(-1)((acc: Int, value: Int) => acc match {
        case -1 => value
        case _ if value - acc == 1 => value
        case _ => 0
      }) > 0
  }
  protected def calculateOutcomeForCombination(cards: (Option[Combination], Option[Combination])): PokerRuleOutcome = {
    cards match {
      case (Some(player1Cards), None) => Some(1, name, player1Cards.toSet)
      case (None, Some(player2Cards)) => Some(2, name, player2Cards.toSet)
      case (Some(player1Cards), Some(player2Cards)) if player1Cards.head._1 > player2Cards.head._1 => Some(1, name, player1Cards.toSet)
      case (Some(player1Cards), Some(player2Cards)) if player1Cards.head._1 < player2Cards.head._1 => Some(2, name, player2Cards.toSet)
      case _ => None
    }
  }
  protected def calculateOutcomeForMultipleCombinations(cards: (Option[(Combination, Combination)], Option[(Combination, Combination)])): PokerRuleOutcome = {
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
}

case class HighestCard() extends PokerRule {
  override def name: String = "Highest Card"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    Range.inclusive(0, 4)
      .map { index => (Some(Seq(player1Hand(index))), Some(Seq(player2Hand(index)))) }
      .map { calculateOutcomeForCombination }
      .filter { _.isDefined }
      .map { case Some(outcome) => outcome }
      .headOption
  }
}

case class Pair() extends PokerRule {
  override def name: String = "Pair"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForCombination((
      extractCombinationOfRepeatedValues(player1Hand, 2),
      extractCombinationOfRepeatedValues(player2Hand, 2)
    ))
  }
}

case class DoublePair() extends PokerRule {
  override def name: String = "Double Pair"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForMultipleCombinations((
      extractMultipleCombinationsOfRepeatedValues(player1Hand, 2,2),
      extractMultipleCombinationsOfRepeatedValues(player2Hand, 2,2)
    ))
  }
}

case class ThreeOfAKind() extends PokerRule {
  override def name: String = "Three of a kind"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForCombination((
      extractCombinationOfRepeatedValues(player1Hand, 3),
      extractCombinationOfRepeatedValues(player2Hand, 3)
    ))
  }
}

case class Straight() extends PokerRule {
  override def name: String = "Straight"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForCombination((
      Some(player1Hand).filter(handHasConsecutiveValues),
      Some(player2Hand).filter(handHasConsecutiveValues)
    ))
  }
}

case class Flush() extends PokerRule {
  override def name: String = "Flush"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForCombination((
      Some(player1Hand).filter(handHasAllCardsSameSuite),
      Some(player2Hand).filter(handHasAllCardsSameSuite)
    ))
  }
}

case class FullHouse() extends PokerRule {
  override def name: String = "Full House"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForMultipleCombinations((
      extractMultipleCombinationsOfRepeatedValues(player1Hand, 3,2),
      extractMultipleCombinationsOfRepeatedValues(player2Hand, 3,2)
    ))
  }
}

case class FourOfAKind() extends PokerRule {
  override def name: String = "Four of a kind"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForCombination((
      extractCombinationOfRepeatedValues(player1Hand, 4),
      extractCombinationOfRepeatedValues(player2Hand, 4)
    ))
  }
}

case class StraightFlush() extends PokerRule {
  override def name: String = "Straight Flush"
  override def evaluateRule(player1Hand: Hand, player2Hand: Hand): PokerRuleOutcome = {
    calculateOutcomeForCombination((
      Some(player1Hand).filter(handHasAllCardsSameSuite).filter(handHasConsecutiveValues),
      Some(player2Hand).filter(handHasAllCardsSameSuite).filter(handHasConsecutiveValues)
    ))
  }
}
