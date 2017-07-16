package conll.model

class CorrectedEssay private[conll](val list: List[Paragraph])

object CorrectedEssay {
  def apply(list: List[Paragraph]): CorrectedEssay = new CorrectedEssay(list)
}
