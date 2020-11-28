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
  }



}
