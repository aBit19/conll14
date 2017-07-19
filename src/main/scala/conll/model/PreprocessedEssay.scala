package conll.model

import conll.util.Corrector

class PreprocessedEssay private(val paragraphsCorrections: List[(Paragraph, List[ConllCorrection])]) {
  override val toString : String = (paragraphsCorrections :\ "") ((pair, string) => pair._1 + string)
  private def corrector: Corrector = Corrector(this)
  def applyCorrections: Option[(String, String)] = {
    val corrections =  corrector.applyCorrections
    if (corrections._2.isRight)
      Some((corrections._1, corrections._2.getOrElse("")))
    else
      None
  }

  def getParagraphs: List[Paragraph] = paragraphsCorrections.map(_._1)
}

object PreprocessedEssay {
  def apply(essay: List[(Paragraph, List[ConllCorrection])]): PreprocessedEssay =
    if (essay == null) throw new NullPointerException("Null PreprocessedEssay")
    else new PreprocessedEssay(essay)
}
