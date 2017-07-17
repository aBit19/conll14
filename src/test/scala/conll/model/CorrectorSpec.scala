package conll.model

import org.scalatest.{FlatSpec, Matchers}
import conll.util.Constants

class CorrectorSpec extends FlatSpec with Matchers {

  "A Corrector" should "accept a PreprocessedEssay" in {
    val corrector: Corrector = Corrector(PreprocessedEssay(List.empty, List.empty))
  }

  it should "be able to apply the corrections and return a (erroneous essay, corrected essay)" in {
    val (errored, corrected): (String, String) = Corrector(PreprocessedEssay(List.empty, List.empty)).applyCorrections
  }

  it should "contain a map (Paragraph, Corrections of the key)" in {
    val map = Corrector(PreprocessedEssay(List.empty, List.empty)).paragraphToCorrections
  }

  "Each correction in a given entry in paragraphToCorrection map" should "refer to the same paragraph" in {
    val preprocessedEssay :PreprocessedEssay = Constants.getPreprocessedEssay
    val paragaphToCorrections = Corrector(preprocessedEssay).paragraphToCorrections
    val listOfCorrections: Iterable[List[ConllCorrection]] = paragaphToCorrections
      .keys
      .map(paragaphToCorrections(_))

    val correctionsReferToTheSameParagraph: Boolean =  listOfCorrections
      .forall(list => Constants.sequentialForAll[ConllCorrection](list, (c1, c2) => c1.paragraph == c2.paragraph))
    correctionsReferToTheSameParagraph shouldEqual  true
  }

  "Each correction in a given entry in paragraphToCorrection map" should "not refer to different paragraph" in {
    val preprocessedEssay :PreprocessedEssay = Constants.getPreprocessedEssay
    val paragaphToCorrections = Corrector(preprocessedEssay).paragraphToCorrections
    val listOfCorrections: Iterable[List[ConllCorrection]] = paragaphToCorrections
      .keys
      .map(paragaphToCorrections(_))

    val correctionsReferToDifferentParagraph: Boolean =  listOfCorrections
      .forall(list => Constants.sequentialForAll[ConllCorrection](list, (c1, c2) => c1.paragraph != c2.paragraph))
    correctionsReferToDifferentParagraph shouldEqual false
  }

}
