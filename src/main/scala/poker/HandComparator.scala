package poker

class HandComparator {
  def compare(firstPlayerHand: String, secondPlayerHand: String): Int = {
    def valuesFromHand(hand: String): Seq[Int] = hand.split(" ").toSeq
      .map(card => cardValueToInt(card.substring(0, 1)))
      .sorted(Ordering.Int.reverse)

    val first = valuesFromHand(firstPlayerHand)
    val second = valuesFromHand(secondPlayerHand)

    Range.inclusive(0, 4)
      .map(index => {
        val f = first(index)
        val s = second(index)
        if (f > s) 1
        else if (f < s) 2
        else 0
      })
      .find(x => x != 0)
      .getOrElse(0)

  }

  private def cardValueToInt(number: String) = {
    if (number == "A") 14
    else if (number == "K") 13
    else if (number == "Q") 12
    else if (number == "J") 11
    else if (number == "T") 10
    else Integer.parseInt(number)
  }
}
