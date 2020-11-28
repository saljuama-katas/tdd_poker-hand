package poker

class HandComparator {
  def compare(firstPlayerHand: String, secondPlayerHand: String): Int = {

    val player1handValues = valuesFromHand(firstPlayerHand)
    val player2handValues = valuesFromHand(secondPlayerHand)

    val player1HasPair = handValuesCounts(player1handValues).values.exists(x => x == 2)
    if (player1HasPair) 1
    else {
      highestCardRule(
        player1handValues,
        player2handValues
      )
    }

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

  private def handValuesCounts(values: Seq[Int]): Map[Int, Int] = {
    values.map { x => x -> values.count(y => x == y) }.toMap
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
