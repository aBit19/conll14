package conll.model

import org.scalatest.{FlatSpec, Matchers}
import conll.util.Constants

class CorrectorSpec extends FlatSpec with Matchers {

  "A Corrector" should "accept a PreprocessedEssay" in {
    val corrector: Corrector = Corrector(PreprocessedEssay(List.empty))
  }

  it should "be able to apply the corrections returning (preprocessedEssay, Either[List[ErrorMessage], CorrectedEssay])"in {
    val (errored, corrected): (String, Either[List[ErrorMessage], String]) =
      Corrector(PreprocessedEssay(List.empty)).applyCorrections
  }
}
