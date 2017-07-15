package conll.model
class PreprocessedEssay private() {
}

object PreprocessedEssay {
  def apply(essay: List[Paragraph], corrections: List[ConllCorrection]): PreprocessedEssay =
    if (essay == null || corrections == null)
      throw new NullPointerException
  else
      new PreprocessedEssay()
}
