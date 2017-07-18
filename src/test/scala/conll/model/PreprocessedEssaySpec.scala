package conll.model

import org.scalatest.{FlatSpec, Matchers}

class PreprocessedEssaySpec extends FlatSpec with Matchers {

  "A PreprocessedEssay" should "be a List of (Paragraph, Corrections to be applied to Paragraph" +
    "ConllCorrections of that essay as annotation" in {
    PreprocessedEssay(List.empty[(Paragraph, List[ConllCorrection])])
  }

  it should "throw an NPE in case null is given as list" in {
    a [NullPointerException] should be thrownBy {
      PreprocessedEssay(null)
    }
  }

  it should "be able to apply its corrections and return an Option[(preprocessed essay, corrected)]" in {
    val essay: Option[(String, String)] = PreprocessedEssay(List.empty).applyCorrections
  }

  it should "have each Correction list to refer to the same paragraph" in {
    val essay: PreprocessedEssay = conll.util.Constants.getPreprocessedEssay
    val referToTheSame = essay
      .paragraphsCorrections
      .zipWithIndex.
      forall(entry => entry._1._2.forall(_.paragraph == entry._2))
  }
}
