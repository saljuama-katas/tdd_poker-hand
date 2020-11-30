package poker

object Suite extends Enumeration {
  type Suite = Value
  val Hearts, Diamonds, Clubs, Spades = Value
}
import Suite._

object PokerDomain {
  type Card = (Int, Suite)
  type Hand = Seq[Card]

  type Combination = Seq[Card]
  type PokerRuleOutcome = Option[(Int, String, Set[Card])]
}
