package conll.model

private[conll] class Corrector private(preprocessedEssay: PreprocessedEssay) {

  private[conll] val paragraphToCorrections: Map[Paragraph, List[ConllCorrection]] = {
    if (preprocessedEssay.paragraphs.isEmpty || preprocessedEssay.corrections.isEmpty) Map.empty
    val paragraphIndexToCorrection: Map[Int, List[ConllCorrection]] =  preprocessedEssay.corrections.groupBy(_.paragraph)
    val paragraphToItsIndex: Map[Paragraph, Int] = preprocessedEssay.paragraphs.zipWithIndex.toMap
    paragraphToItsIndex.map(pair => (pair._1, paragraphIndexToCorrection.get(pair._2))).mapValues(_.get)
  }

  def applyCorrections: (String, String) = (preprocessed, corrected)

  private val preprocessed: String = Corrector.makeStringEssayFromParagraphs(preprocessedEssay.paragraphs)
  private def corrected : String = Corrector.makeStringEssayFromParagraphs(correctEssay())

  private def correctEssay(): List[Paragraph] =
    paragraphToCorrections.toList.map(pair => correctParagraph(pair._1, pair._2))

  private def correctParagraph(paragraph: Paragraph, corrections: List[ConllCorrection]): Paragraph = {
    val stringBuilder = new StringBuilder(paragraph)
    val sorted = corrections.sorted.reverse
    var diff: Int = 0
    for (conllCorrection <- sorted) {
      var start_off: Int = 0
      var start_end: Int = 0
      var correction: String = ""
      try {
        start_off  = conllCorrection.errorSpan._1
        start_end = conllCorrection.errorSpan._2
        correction = conllCorrection.correction
        println(s"Deletes [${start_off + diff} - ${start_end + diff}]")
        stringBuilder.delete(start_off + diff, start_end + diff)
        println(s"Inserts to [${start_off + diff}] : $correction")
        stringBuilder.insert(start_off + diff, correction)
        print(s"diff = $diff + (${correction.length} - ($start_end - $start_off))")
        diff = diff + (correction.length - (start_end - start_off))
        println(s"= ${diff}")
      } catch {
        case ex: Exception => {
          println(s"$start_off - $start_end - $correction")
        }
      }
    }
    stringBuilder.toString()
  }

}

private[conll] object Corrector {
  def apply(preprocessedEssay: PreprocessedEssay): Corrector = new Corrector(preprocessedEssay)
  def makeStringEssayFromParagraphs(list: List[Paragraph]): String = (list:\ "")(_ .trim + _.trim)
}
