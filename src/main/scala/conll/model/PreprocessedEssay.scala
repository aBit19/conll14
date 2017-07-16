package conll.model
class PreprocessedEssay private(val paragraphs: List[Paragraph], val corrections: List[ConllCorrection]) {


  override def toString: String = paragraphs.toString()
}

object PreprocessedEssay {
  def apply(essay: List[Paragraph], corrections: List[ConllCorrection]): PreprocessedEssay =
    if (essay == null || corrections == null)
      throw new NullPointerException
  else
      new PreprocessedEssay(essay, corrections)
}
