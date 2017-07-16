package conll.model


import org.scalatest.{FlatSpec, Matchers}

class ConllCorrectionSpec extends FlatSpec with Matchers {
  "A ConllCorrection" should "accept the index of the paragraph the correction is to be applied to. " +
    "Note: Paragraphs are counted starting from zero" in {
    ConllCorrection(0, (0, 1), "")
  }

  it should "throw IAE in case negative index is given as paragraph" in {
    an [IllegalArgumentException] should be thrownBy {
      ConllCorrection(-1, (1, 2), "")
    }
  }

  it should "be able to accept a Span instance representing the error span" in {
    ConllCorrection(1, (1, 2), "")
  }

  it should "throw an IAE in case a negative index is give to the Span" in {
    an [IllegalArgumentException] should be thrownBy {
      ConllCorrection(1, (-1, 0), "")
    }
  }

  it should "throw an IAE in case the first index of the span is larger than the second" in {
    an [IllegalArgumentException] should be thrownBy {
      ConllCorrection(0, (4, 4), "")
    }
  }

  it should "be able to accept a correction represented by a string" in {
    ConllCorrection(0, (1, 3), "the")
  }

  it should "throw an IAE in case is given null as correction" in {
    an [IllegalArgumentException] should be thrownBy {
      ConllCorrection(0, (1, 3), null)
    }
  }

  it should "extend ordered trait" in {
    val correction: Ordered[ConllCorrection] = ConllCorrection(0, (1, 3), "")
  }

}
