package conll.util

import conll.model.ConllCorrection
import org.scalatest.{FlatSpec, Matchers}

class CorrectionCleanerSpec extends FlatSpec with Matchers{

  "A CorrectionCleaner" should "accept a list of ConllCorrections " in {
    val list: List[ConllCorrection] = CorrectionCleaner.clean(List.empty[ConllCorrection])
  }

  it should "throw a NPE in case a null list is given" in {
    a [NullPointerException] should be thrownBy {
      CorrectionCleaner.clean(null)
    }
  }

  it should "reduce two correction starting from the same point and one of them is empty and the other has " +
    "a correction" in {
    val list = CorrectionCleaner.clean(getCorrectionsToBeReduced)
    list should have length 2
  }

  it should "remove an empty correction from the list when there is no other correction " +
    "in that list that refers the same paragraph point" in {
    val list = CorrectionCleaner.clean(ConllCorrection(0, (1, 4), "") :: getCorrectionsToBeReduced)
    list should have length 2
  }

  it should "contain List of corrections in order: for example let cor1, cor2 be two conllCorrections if cor1" +
    ".start_off  > cor2.start_off then cor1 should come first in the list also " in {
    val list = CorrectionCleaner.clean(CorrectionCleaner.clean(ConllCorrection(0, (1, 4), "") ::
      getCorrectionsToBeReduced))
    list.map(_.errorSpan._1).reverse shouldBe sorted
  }

  /* it should "not contain two corrections starting from the same point in a given paragraph" in {
    conll.util.Constants.getPreprocessedEssay
      .paragraphsCorrections
      .foreach(p => p._2.groupBy(_.errorSpan._1)
        .values.foreach(e => e should have length 1))
  }*/

  private val getCorrectionsToBeReduced: List[ConllCorrection] = {
    List(
      ConllCorrection(0, (120, 124), "The workplace"),
      ConllCorrection(0, (120, 131), ""),
      ConllCorrection(3, (1, 5), " ")
    )
  }


}
