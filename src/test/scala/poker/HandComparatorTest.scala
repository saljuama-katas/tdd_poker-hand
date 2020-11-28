package poker

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HandComparatorTest extends AnyWordSpec with Matchers {
  "The Hand Comparator" can {
    val comparator = new HandComparator()
    "use the High Card rule" when {
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
    "use the Pair rule" when {
      "one player has a pair, then wins" in {
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
  }
}
