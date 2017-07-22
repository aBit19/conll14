package conll.util

import conll.model.{ErrorMessage, PreprocessedEssay}
import org.scalatest.{FlatSpec, Matchers}

class CorrectorSpec extends FlatSpec with Matchers {

  "A Corrector" should "accept a PreprocessedEssay" in {
    val corrector: EssayCorrector = EssayCorrector(PreprocessedEssay(List.empty))
  }

  it should "be able to apply the corrections returning (preprocessedEssay, Either[List[ErrorMessage], CorrectedEssay])"in {
    val (errored, corrected): (String, Either[List[ErrorMessage], String]) =
      EssayCorrector(PreprocessedEssay(List.empty)).applyCorrections
  }
}
