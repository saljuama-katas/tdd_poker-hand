package poker

class HandComparator {
  def compare(firstPlayerHand: String, secondPlayerHand: String): Int = {

    highestCardRule(
      valuesFromHand(firstPlayerHand),
      valuesFromHand(secondPlayerHand)
    )
  }

  private def highestCardRule(first: Seq[Int], second: Seq[Int]): Int = {
    Range.inclusive(0, 4)
      .map { index => (first(index), second(index)) }
      .map {
        case (f, s) if f > s => 1
        case (f, s) if f < s => 2
        case _ => 0
      }
      .find(x => x != 0)
      .getOrElse(0)
  }

  private def valuesFromHand(hand: String): Seq[Int] = {
    hand
      .split(" ")
      .map(card => cardValueToInt(card.substring(0, 1)))
      .sorted(Ordering.Int.reverse)
  }

  private def cardValueToInt(number: String): Int = {
    Map("A" -> 14, "K" -> 13, "Q" -> 12, "J" -> 11, "T" -> 10)
      .getOrElse(number, Integer.parseInt(number))
  }
}
