package poker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HandComparatorTest extends AnyWordSpec with Matchers {
  "The Hand Comparator" can {
    val comparator = new HandComparator()
    "apply the High Card rule" when {
      "highest card in both hands is different" in {
        val winner = comparator.compare("2H 3D 4S 5C AD", "2D 3H 4C 5S KD")
        winner mustBe 1
      }
      "second highest card when first card is tied" in {
        val winner = comparator.compare("2H 3D 4S 5C AD", "2D 3H 4C 6S AS")
        winner mustBe 2
      }
      "last card when all previous cards are tied" in {
        val winner = comparator.compare("3H JD QS KC AD", "2D JH QC KS AS")
        winner mustBe 1
      }
      "tie when both hands have the same values" in {
        val winner = comparator.compare("2H 3D 4S 5C 6D", "2D 3H 4C 5S 6S")
        winner mustBe 0
      }
    }
    "apply the Pair rule" when {
      "pair beats highest card" in {
        val winner = comparator.compare("2H 2D 3S 4C 5D", "2C 3H 4C 5S AD")
        winner mustBe 1
      }
      "both player have a pair, the one with higher pair wins" in {
        val winner = comparator.compare("2H 2D 3S 4C 5D", "2C 3H 3C 4S 5D")
        winner mustBe 2
      }
      "both players have a pair of equal value, the one with the highest card wins" in {
        val winner = comparator.compare("2H 2D 3S 4C AD", "2C 2S 3C 4S 5D")
        winner mustBe 1
      }
    }
    "apply the Double Pair rule" when {
      "double pair beats pair" in {
        val winner = comparator.compare("2H 2D 3S 3C 5D", "2C 3H 4C AS AD")
        winner mustBe 1
      }
      "both players with double pair, the highest pair wins" in {
        val winner = comparator.compare("3H 3D 4C JS JD", "2H 2D 4S AC AD")
        winner mustBe 2
      }
      "both players with double pair, and equal highest pair, then 2nd highest pair wins" in {
        val winner = comparator.compare("AS AH 3H 3D 4C", "AC AD 2H 2D KS")
        winner mustBe 1
      }
    }
    "apply the Three of a Kind rule" when {
      "three of a kind beats double pair" in {
        val winner = comparator.compare("AH AD KS KC QD", "JC JH JC 2S 3D")
        winner mustBe 2
      }
      "both players with three of a kind, the highest wins" in {
        val winner = comparator.compare("JH JD JS 2C 3D", "8C 8H 8C QS KD")
        winner mustBe 1
      }
    }
  }
}
