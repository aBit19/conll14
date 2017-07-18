package conll.model


private[conll] class Corrector private(preprocessedEssay: PreprocessedEssay) {

  private val preprocessed: String = Corrector.makeStringEssayFromParagraphs(preprocessedEssay.getParagraphs)

  def applyCorrections: (String, Either[List[ErrorMessage], String]) = (preprocessed, corrected)

  private def corrected : Either[List[ErrorMessage], String] = {
    val correctedPar: List[Either[ErrorMessage, Paragraph]] = correctEssay()
    if (!correctedPar.exists(_.isLeft))
      Right(Corrector.concatParagraphs(correctedPar))
    else
      Left(Corrector.getErrors(correctedPar))
  }

  private def correctEssay(): List[Either[ErrorMessage, Paragraph]] =
    preprocessedEssay.paragraphsCorrections.map(pair => Corrector.correctParagraph(pair._1, pair._2.head, pair._2.tail))

}

private[conll] object Corrector {
  def apply(preprocessedEssay: PreprocessedEssay): Corrector = new Corrector(preprocessedEssay)
  def makeStringEssayFromParagraphs(list: List[Paragraph]): String = (list:\ "")(_ .trim + _.trim)

  private def correctParagraph(paragraph: Paragraph,
                               headCorrection: ConllCorrection,
                               corrections: List[ConllCorrection]):
  Either[ErrorMessage, Paragraph] = {
    val stringBuilder = new StringBuilder(paragraph)
    if (correctionSpansTwoParagraphs(headCorrection))
      handleCorrectionSpanningTwoParagraphs(paragraph, headCorrection, stringBuilder)
    try {
      for (conllCorrection <- corrections) {
        applyCorrectionMutatingStringBuilder(conllCorrection, stringBuilder)
      }
      Right(stringBuilder.result().replaceAll("  ", " "))
    } catch {
      case ex: Exception => Left(s"${ex.getMessage} in: ($paragraph")
    }
  }

  private def correctionSpansTwoParagraphs(headCorrection: ConllCorrection): Boolean = headCorrection.errorSpan._2 == 0
  private def handleCorrectionSpanningTwoParagraphs(paragraph: Paragraph, headCorrection: ConllCorrection,
                                                     stringBuilder: StringBuilder): Unit = {
    applyCorrectionMutatingStringBuilder(
      ConllCorrection(0, (headCorrection.errorSpan._1, paragraph.length), headCorrection.correction),
      stringBuilder)
  }

  private def applyCorrectionMutatingStringBuilder(conllCorrection: ConllCorrection, stringBuilder: StringBuilder): Unit = {
    val (start_off, start_end) = conllCorrection.errorSpan
    stringBuilder.delete(start_off, start_end)
    stringBuilder.insert(start_off, " " + conllCorrection.correction)
  }

  private def concatParagraphs(list: List[Either[ErrorMessage, Paragraph]]): String = {
    //Assumption the list contains only Right
    ("" /: list) ((paragraph: Paragraph, either) => either.getOrElse("") + paragraph)
  }

  private def getErrors(either: List[Either[ErrorMessage, Paragraph]]): List[ErrorMessage] = {
    def helper(rem: List[Either[ErrorMessage, Paragraph]], res: List[ErrorMessage]): List[ErrorMessage] =
      rem match {
        case Nil => res
        case head :: tail if head.isLeft => helper(tail, head.swap.getOrElse("") :: res)
        case _ :: tail => helper(tail, res)
      }
    helper(either, Nil)
  }
}
