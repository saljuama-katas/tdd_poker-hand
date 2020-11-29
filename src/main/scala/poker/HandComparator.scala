package poker

import poker.InputParser.parseHand

class HandComparator {
  private val prioritizedRules: Seq[PokerRule] = Seq(
    StraightFlush(),
    FourOfAKind(),
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
