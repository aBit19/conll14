package conll.model

import org.scalatest.{FlatSpec, Matchers}

class PreprocessedEssaySpec extends FlatSpec with Matchers {

  "A PreprocessedEssay" should "accept a List of string paragraphs representing the essay and a list of " +
    "ConllCorrections of that essay as annotation" in {
    val preprocessedEssay = PreprocessedEssay(List.empty[Paragraph], List.empty[ConllCorrection])
  }

  it should "throw an NPE in case null is given as list of Paragraphs" in {
    a [NullPointerException] should be thrownBy {
      PreprocessedEssay(null, List.empty)
    }
  }

  it should "throw an NPE in case null is given as list of Corrections" in {
    a [NullPointerException] should be thrownBy {
      PreprocessedEssay(List.empty, null)
    }
  }
}
