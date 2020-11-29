package poker

trait PokerRule {
  def name: String
  def rule(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Int
}

case class HighestCardRule() extends PokerRule {
  override def name = "Highest Card"
  override def rule(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Int = {
    Range.inclusive(0, 4)
      .map { index => (player1HandValues(index), player2HandValues(index)) }
      .map {
        case (f, s) if f > s => 1
        case (f, s) if f < s => 2
        case _ => 0
      }
      .find(x => x != 0)
      .getOrElse(0)
  }
}

case class PairRule() extends PokerRule {
  override def name = "Pair"
  override def rule(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Int = {
    (handPairValue(player1HandValues), handPairValue(player2HandValues)) match {
      case (f, s) if f > s => 1
      case (f, s) if f < s => 2
      case _ => 0
    }
  }
  private def handPairValue(values: Seq[Int]): Int = {
    values
      .map { x => x -> values.count(y => x == y) }
      .find { _._2 == 2 }
      .map { _._1}
      .getOrElse(0)
  }
}

case class DoublePairRule() extends PokerRule {
  override def name = "Double Pair"
  override def rule(player1HandValues: Seq[Int], player2HandValues: Seq[Int]): Int = {
    (handDoublePairValues(player1HandValues), handDoublePairValues(player2HandValues)) match {
      case ((f1, _), (s1, _)) if f1 > s1 => 1
      case ((f1, _), (s1, _)) if f1 < s1 => 2
      case ((f1, f2), (s1, s2)) if f1 == s1 && f2 > s2 => 1
      case ((f1, f2), (s1, s2)) if f1 == s1 && f2 < s2 => 2
      case _ => 0
    }
  }

  private def handDoublePairValues(values: Seq[Int]): (Int, Int) = {
    val pairs = values
      .map { x => x -> values.count(y => x == y) }
      .filter { _._2 == 2 }
      .distinct
      .map { _._1 }
      .sorted { Ordering.Int.reverse }

    pairs match {
      case Seq(pair1Value, pair2Value) => (pair1Value, pair2Value)
      case _ => (0, 0)
    }
  }
}

class HandComparator {
  def compare(firstPlayerHand: String, secondPlayerHand: String): Int = {
    val rules: Seq[PokerRule] = Seq(
      DoublePairRule(),
      PairRule(),
      HighestCardRule()
    )
    val player1HandValues = handValues(firstPlayerHand)
    val player2HandValues = handValues(secondPlayerHand)

    rules
      .map { rule => rule.rule(player1HandValues, player2HandValues) }
      .find { _ > 0 }
      .getOrElse(0)
  }

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
