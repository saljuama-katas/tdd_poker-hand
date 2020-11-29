package poker

object Suite extends Enumeration {
  type Suite = Value
  val Hearts, Diamonds, Clubs, Spades = Value

  def fromString(str: String): Suite = {
    str match {
      case "H" => Hearts
      case "D" => Diamonds
      case "C" => Clubs
      case "S" => Spades
    }
  }
}
import Suite._

object HandComparator {
  type Card = (Int, Suite)
  type Hand = Seq[Card]
  private def parseHand(handString: String): Hand = {
    def parseCard(cardString: String): Card = {
      def cardValueToInt(number: String): Int = {
        Map("A" -> 14, "K" -> 13, "Q" -> 12, "J" -> 11, "T" -> 10)
          .getOrElse(number, Integer.parseInt(number))
      }
      (
        cardValueToInt(cardString.substring(0,1)),
        Suite.fromString(cardString.substring(1, 2))
      )
    }
    handString
      .split(" ")
      .map { parseCard }
      .sortBy { _._1 }
      .reverse
  }
}
import HandComparator._

class HandComparator {
  private val prioritizedRules: Seq[PokerRule] = Seq(
    FullHouse(),
    Flush(),
    Straight(),
    ThreeOfAKind(),
    DoublePair(),
    Pair(),
    HighestCard()
  )

  def compare(firstPlayerHand: String, secondPlayerHand: String): Option[Int] = {
    val player1Hand = parseHand(firstPlayerHand)
    val player2Hand = parseHand(secondPlayerHand)
    prioritizedRules
      .map { rule => rule.findWinner(player1Hand, player2Hand) }
      .filter { _.isDefined }
      .map { case Some(player) => player }
      .headOption
  }
}

trait PokerRule {
  def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int]

  protected def cardValuesInHand(hand: Hand): Seq[Int] = hand.map { _._1 }
  protected def findValueNTimes(hand: Hand, times: Int): Option[Int] = {
    val values = cardValuesInHand(hand)
    values
      .map { cardValue: Int  => cardValue -> values.count(y => cardValue == y) }
      .find { _._2 == times }
      .map { _._1 }
  }
  protected def playerWithHighestValue(values: (Option[Int], Option[Int])): Option[Int] = {
    values match {
      case (Some(player1), Some(player2)) if player1 > player2 => Some(1)
      case (Some(player1), Some(player2)) if player1 < player2 => Some(2)
      case (Some(_), None) => Some(1)
      case (None, Some(_)) => Some(2)
      case _ => None
    }
  }
}
case class HighestCard() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    val player1HandValues = cardValuesInHand(player1Hand)
    val player2HandValues = cardValuesInHand(player2Hand)
    Range.inclusive(0, 4)
      .map { index => (Some(player1HandValues(index)), Some(player2HandValues(index))) }
      .map { playerWithHighestValue }
      .filter { _.isDefined }
      .map { case Some(player) => player }
      .headOption
  }
}
case class Pair() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    playerWithHighestValue((
      findValueNTimes(player1Hand, 2),
      findValueNTimes(player2Hand, 2)
    ))
  }
}
case class DoublePair() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    (handDoublePairValues(player1Hand), handDoublePairValues(player2Hand)) match {
      case ((f1, _), (s1, _)) if f1 > s1 => Some(1)
      case ((f1, _), (s1, _)) if f1 < s1 => Some(2)
      case ((f1, f2), (s1, s2)) if f1 == s1 && f2 > s2 => Some(1)
      case ((f1, f2), (s1, s2)) if f1 == s1 && f2 < s2 => Some(2)
      case _ => None
    }
  }
  private def handDoublePairValues(hand: Hand): (Int, Int) = {
    val values = cardValuesInHand(hand)
    val pairs = values
      .map { x => x -> values.count(y => x == y) }
      .distinct
      .filter { _._2 == 2 }
      .map { _._1 }
      .sorted { Ordering.Int.reverse }

    pairs match {
      case Seq(pair1Value, pair2Value) => (pair1Value, pair2Value)
      case _ => (0, 0)
    }
  }
}
case class ThreeOfAKind() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    playerWithHighestValue((
      findValueNTimes(player1Hand, 3),
      findValueNTimes(player2Hand, 3)
    ))
  }
}
case class Straight() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    playerWithHighestValue(
      playerHasConsecutiveValues(player1Hand),
      playerHasConsecutiveValues(player2Hand)
    )
  }
  private def playerHasConsecutiveValues(hand: Hand): Option[Int] = {
    val highestInConsecutive = cardValuesInHand(hand)
      .sorted
      .fold(-1)((acc, value) => acc match {
        case -1 => value
        case _ if value - acc == 1 => value
        case _ => 0
      })
    if (highestInConsecutive > 0) Some(highestInConsecutive) else None
  }
}
case class Flush() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    playerWithHighestValue(
      highestCardWithFlush(player1Hand),
      highestCardWithFlush(player2Hand)
    )
  }
  private def highestCardWithFlush(hand: Hand): Option[Int] = {
    if (hand.map { _._2 }.distinct.length != 1) None
    else hand.map { _._1 }.headOption
  }
}
case class FullHouse() extends PokerRule {
  override def findWinner(player1Hand: Hand, player2Hand: Hand): Option[Int] = {
    (handFullHouseValues(player1Hand), handFullHouseValues(player2Hand)) match {
      case ((Some(_), Some(_)), (None, None)) => Some(1)
      case ((None, None), (Some(_), Some(_))) => Some(2)
      case ((Some(p13), Some(_)), (Some(p23), Some(_))) if p13 > p23 => Some(1)
      case ((Some(p13), Some(_)), (Some(p23), Some(_))) if p13 < p23 => Some(2)
      case _ => None
    }
  }
  private def handFullHouseValues(hand: Hand): (Option[Int], Option[Int]) = {
    val values = cardValuesInHand(hand)
    val valueCounts = values
      .map { x => x -> values.count(y => x == y) }
      .distinct
      .sortBy { t => (-t._2, -t._1) }
    (
      valueCounts.find {_._2 == 3}.map {_._1},
      valueCounts.find {_._2 == 2}.map {_._1}
    )
  }
}