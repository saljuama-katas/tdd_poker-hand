package poker

import PokerDomain._
import Suite._

object InputParser {
  def parseHand(handString: String): Hand = handString
    .split(" ")
    .map { parseCard }
    .sortBy { _._1 }
    .reverse

  private def parseCard(cardString: String): Card = (
    cardValue(cardString.substring(0,1)),
    cardSuite(cardString.substring(1, 2))
  )

  private def cardValue(number: String): Int = {
    Map(
      "A" -> 14,
      "K" -> 13,
      "Q" -> 12,
      "J" -> 11,
      "T" -> 10
    ).getOrElse(number, Integer.parseInt(number))
  }

  private def cardSuite(str: String): Suite = {
    str match {
      case "H" => Hearts
      case "D" => Diamonds
      case "C" => Clubs
      case "S" => Spades
    }
  }
}
