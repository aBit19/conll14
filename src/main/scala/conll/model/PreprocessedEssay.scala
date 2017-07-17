package conll.model

class PreprocessedEssay private(val paragraphs: List[Paragraph], val corrections: List[ConllCorrection]) {
  override val toString : String = (paragraphs :\ "") (_.trim + _.trim)
  private def corrector: Corrector = Corrector(this)
  def applyCorrections = corrector.applyCorrections
}

object PreprocessedEssay {
  def apply(essay: List[Paragraph], corrections: List[ConllCorrection]): PreprocessedEssay =
    if (essay == null || corrections == null)
      throw new NullPointerException
  else
      new PreprocessedEssay(essay, corrections)
}
