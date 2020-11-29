package poker

trait PokerRule {
  def findWinner(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Option[Int]

  protected def findValueNTimes(values: Seq[Int], times: Int): Option[Int] = {
    values
      .map { x => x -> values.count(y => x == y) }
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
  override def findWinner(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Option[Int] = {
    Range.inclusive(0, 4)
      .map { index => (Some(player1HandValues(index)), Some(player2HandValues(index))) }
      .map { playerWithHighestValue }
      .filter { _.isDefined }
      .map { case Some(player) => player }
      .headOption
  }
}
case class Pair() extends PokerRule {
  override def findWinner(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Option[Int] = {
    playerWithHighestValue((
      findValueNTimes(player1HandValues, 2),
      findValueNTimes(player2HandValues, 2)
    ))
  }
}
case class DoublePair() extends PokerRule {
  override def findWinner(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Option[Int] = {
    (handDoublePairValues(player1HandValues), handDoublePairValues(player2HandValues)) match {
      case ((f1, _), (s1, _)) if f1 > s1 => Some(1)
      case ((f1, _), (s1, _)) if f1 < s1 => Some(2)
      case ((f1, f2), (s1, s2)) if f1 == s1 && f2 > s2 => Some(1)
      case ((f1, f2), (s1, s2)) if f1 == s1 && f2 < s2 => Some(2)
      case _ => None
    }
  }
  private def handDoublePairValues(values: Seq[Int]): (Int, Int) = {
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
  override def findWinner(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Option[Int] = {
    playerWithHighestValue((
      findValueNTimes(player1HandValues, 3),
      findValueNTimes(player2HandValues, 3)
    ))
  }
}
case class Straight() extends PokerRule {
  override def findWinner(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Option[Int] = {
    playerWithHighestValue(
      playerHasConsecutiveValues(player1HandValues),
      playerHasConsecutiveValues(player2HandValues)
    )
  }
  private def playerHasConsecutiveValues(values: Seq[Int]): Option[Int] = {
    val highestInConsecutive = values
      .sorted
      .fold(-1)((acc, value) => acc match {
        case -1 => value
        case _ if value - acc == 1 => value
        case _ => 0
      })
    if (highestInConsecutive > 0) Some(highestInConsecutive) else None
  }
}

class HandComparator {
  def compare(firstPlayerHand: String, secondPlayerHand: String): Option[Int] = {
    val player1HandValues = handValues(firstPlayerHand)
    val player2HandValues = handValues(secondPlayerHand)
    prioritizedRules
      .map { rule => rule.findWinner(player1HandValues, player2HandValues) }
      .filter { _.isDefined }
      .map { case Some(player) => player }
      .headOption
  }

  private def prioritizedRules: Seq[PokerRule] = Seq(
    Straight(),
    ThreeOfAKind(),
    DoublePair(),
    Pair(),
    HighestCard()
  )

  private def handValues(hand: String): Seq[Int] = {
    def cardValueToInt(number: String): Int = {
      Map("A" -> 14, "K" -> 13, "Q" -> 12, "J" -> 11, "T" -> 10)
        .getOrElse(number, Integer.parseInt(number))
    }
    hand
      .split(" ")
      .map(card => cardValueToInt(card.substring(0, 1)))
      .sorted(Ordering.Int.reverse)
  }
}
