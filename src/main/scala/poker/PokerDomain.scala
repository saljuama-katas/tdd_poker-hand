package poker

object Suite extends Enumeration {
  type Suite = Value
  val Hearts, Diamonds, Clubs, Spades = Value
}
import Suite._

object PokerDomain {
  type Card = (Int, Suite)
  type Hand = Seq[Card]
}
