package poker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import poker.Suite._

class HandComparatorTest extends AnyWordSpec with Matchers {
  "The Hand Comparator" can {
    val comparator = new HandComparator()
    "apply the High Card rule" when {
      "highest card in both hands is different" in {
        val winner = comparator.compare("2H 3D 4S 5C AD", "2D 3H 4C 5S KD")
        winner mustBe Some(1, "Highest Card", Set((14, Diamonds)))
      }
      "second highest card when first card is tied" in {
        val winner = comparator.compare("2H 3D 4S 5C AD", "2D 3H 4C 6S AS")
        winner mustBe Some(2, "Highest Card", Set((6, Spades)))
      }
      "last card when all previous cards are tied" in {
        val winner = comparator.compare("3H JD QS KC AD", "2D JH QC KS AS")
        winner mustBe Some(1, "Highest Card", Set((3, Hearts)))
      }
      "tie when both hands have the same values" in {
        val winner = comparator.compare("2H 3D 4S 5C 6D", "2D 3H 4C 5S 6S")
        winner mustBe None
      }
    }
    "apply the Pair rule" when {
      "pair beats highest card" in {
        val winner = comparator.compare("2H 2D 3S 4C 5D", "2C 3H 4C 5S AD")
        winner mustBe Some(1, "Pair", Set((2, Hearts), (2, Diamonds)))
      }
      "both player have a pair, the one with higher pair wins" in {
        val winner = comparator.compare("2H 2D 3S 4C 5D", "2C 3H 3C 4S 5D")
        winner mustBe Some(2, "Pair", Set((3, Hearts), (3, Clubs)))
      }
      "both players have a pair of equal value, the one with the highest card wins" in {
        val winner = comparator.compare("2H 2D 3S 4C AD", "2C 2S 3C 4S 5D")
        winner mustBe Some(1, "Highest Card", Set((14, Diamonds)))
      }
    }
    "apply the Double Pair rule" when {
      "double pair beats pair" in {
        val winner = comparator.compare("2H 2D 3S 3C 5D", "2C 3H 4C AS AD")
        winner mustBe Some(1, "Double Pair", Set((3, Spades), (3, Clubs), (2, Hearts), (2, Diamonds)))
      }
      "both players with double pair, the highest pair wins" in {
        val winner = comparator.compare("3H 3D 4C JS JD", "2H 2D 4S AC AD")
        winner mustBe Some(2, "Double Pair", Set((14, Clubs), (14, Diamonds), (2, Hearts), (2, Diamonds)))
      }
      "both players with double pair, and equal highest pair, then 2nd highest pair wins" in {
        val winner = comparator.compare("AS AH 3H 3D 4C", "AC AD 2H 2D KS")
        winner mustBe Some(1, "Double Pair", Set((14, Spades), (14, Hearts), (3, Hearts), (3, Diamonds)))
      }
      "oth players with double pair, equal pairs, then highest card wins" in {
        val winner = comparator.compare("AS AH KC KD QS", "AC AD KS KH JS")
        winner mustBe Some(1, "Highest Card", Set((12, Spades)))
      }
    }
    "apply the Three of a Kind rule" when {
      "three of a kind beats double pair" in {
        val winner = comparator.compare("AH AD KS KC QD", "JC JH JD 2S 3D")
        winner mustBe Some(2, "Three of a kind", Set((11, Clubs), (11, Hearts), (11, Diamonds)))
      }
      "both players with three of a kind, the highest wins" in {
        val winner = comparator.compare("JH JD JS 2C 3D", "8C 8H 8C QS KD")
        winner mustBe Some(1, "Three of a kind", Set((11, Spades), (11, Hearts), (11, Diamonds)))
      }
    }
    "apply the Straight rule" when {
      "straight beats three of a kind" in {
        val winner = comparator.compare("2H 3D 4C 5S 6H", "AH AC AD KS QH")
        winner mustBe Some(1, "Straight", Set((2, Hearts),(3, Diamonds),(4, Clubs),(5, Spades),(6, Hearts)))
      }
      "both players with straight, the highest card wins" in {
        val winner = comparator.compare("2H 3D 4C 5S 6H", "3H 4S 5D 6S 7C")
        winner mustBe Some(2, "Straight", Set((3, Hearts),(4, Spades),(5, Diamonds),(6, Spades),(7, Clubs)))
      }
    }
    "apply the Flush rule" when {
      "flush beats straight" in {
        val winner = comparator.compare("AH KS QD JS TC", "2S 3S 7S 8S 9S")
        winner mustBe Some(2, "Flush", Set())
      }
      "both players with flush, the highest card wins" in {
        val winner = comparator.compare("3H 4H 5H 6H AH", "2S 3S 7S 8S 9S")
        winner mustBe Some(1, "Flush", Set())
      }
    }
    "apply the Full House rule" when {
      "full house beats flush" in {
        val winner = comparator.compare("7H 7C 7S 8H 8D", "3H 4H 5H 6H AH")
        winner mustBe Some(1, "Full House", Set((7, Hearts),(7, Clubs),(7, Spades),(8, Hearts),(8, Diamonds)))
      }
      "both players with full house, the highest three of a kind wins" in {
        val winner = comparator.compare("7H 7C 7S AH AD", "9H 9C 9S JC JD")
        winner mustBe Some(2, "Full House", Set((9, Hearts),(9, Clubs),(9, Spades),(11, Clubs),(11, Diamonds)))
      }
    }
    "apply the Four of a Kind rule" when {
      "Four of a kind beats full house" in {
        val winner = comparator.compare("7H 7C 7S 8H 8D", "3H 3D 3C 3S 4H")
        winner mustBe Some(2, "Four of a kind", Set((3, Hearts), (3, Diamonds), (3, Clubs), (3, Spades)))
      }
      "both players with Four of a Kind, the highest four of a kind wins" in {
        val winner = comparator.compare("4H 4C 4S 4D 8D", "3H 3D 3C 3S AH")
        winner mustBe Some(1, "Four of a kind", Set((4, Hearts), (4, Diamonds), (4, Clubs), (4, Spades)))
      }
    }
    "apply the Straight Flush rule" when {
      "Straight Flush beads Four of a kind" in {
        val winner = comparator.compare("2S 3S 4S 5S 6S", "AH AD AC AS 4H")
        winner mustBe Some(1, "Straight Flush", Set())
      }
      "both players with a Straight Flush, the highest card wins" in {
        val winner = comparator.compare("2S 3S 4S 5S 6S", "3D 4D 5D 6D 7D")
        winner mustBe Some(2, "Straight Flush", Set())
      }
    }
  }
}
