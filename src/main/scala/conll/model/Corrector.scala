package conll.model

private[conll] class Corrector private(preprocessedEssay: PreprocessedEssay) {

  private[conll] val paragraphToCorrections: Map[Paragraph, List[ConllCorrection]] = {
    val paragraphIndexToCorrection: Map[Int, List[ConllCorrection]] =  preprocessedEssay.corrections.groupBy(_.paragraph)
    val paragraphToItsIndex: Map[Paragraph, Int] = preprocessedEssay.paragraphs.zipWithIndex.toMap
    paragraphToItsIndex
      .map(pair => (pair._1, paragraphIndexToCorrection.get(pair._2)))
      .filter(_._2.isDefined)
      .mapValues(_.get)
  }

  def applyCorrections: (String, Either[List[ErrorMessage], String]) = (preprocessed, corrected)

  private val preprocessed: String = Corrector.makeStringEssayFromParagraphs(preprocessedEssay.paragraphs)

  private def corrected : Either[List[ErrorMessage], String] = {
    val correctedPar: List[Either[ErrorMessage, Paragraph]] = correctEssay
    if (!correctedPar.exists(_.isLeft))
      Right((correctedPar :\ "") ((either, par) => either.getOrElse("") + par))
    else
      Left(correctedPar.filter(_.isLeft).map(_.getOrElse("")))
  }
  private def correctEssay: List[Either[ErrorMessage, Paragraph]] =
    paragraphToCorrections.toList.map(pair => correctParagraph(pair._1, pair._2))

  private def correctParagraph(paragraph: Paragraph, corrections: List[ConllCorrection]):
  Either[ErrorMessage, Paragraph] = {
    import Corrector._
    var start_off, start_end: Int = 0
    var correction = ""
    val stringBuilder = new StringBuilder(paragraph)
    try {
      for (conllCorrection <- prepareCorrections(corrections)) {
        start_off = conllCorrection.errorSpan._1
        start_end = conllCorrection.errorSpan._2
        correction = conllCorrection.correction
        stringBuilder.delete(start_off, start_end)
        stringBuilder.insert(start_off, " " + correction)
      }
      Right(stringBuilder.result().replaceAll("  ", " "))
    } catch {
      case ex: Exception => Left(s"${ex.getMessage} in ([$start_off - $start_end]: $correction-> $paragraph")
    }
  }

}

private[conll] object Corrector {
  def apply(preprocessedEssay: PreprocessedEssay): Corrector = new Corrector(preprocessedEssay)
  def makeStringEssayFromParagraphs(list: List[Paragraph]): String = (list:\ "")(_ .trim + _.trim)

  private[conll] def prepareCorrections(conllCorrections: List[ConllCorrection]): List[ConllCorrection] = {
    val reducedDuplicates: List[ConllCorrection] = conllCorrections
      .groupBy(_.errorSpan._1)
      .iterator
      .filter(_._2.size > 1)
      .map(pair => reduceDuplicates(pair._2)).toList

    val indexToFilter = reducedDuplicates.map(_.errorSpan._1).toSet
    val noDuplicates = conllCorrections.filter(corr => !indexToFilter.contains(corr.errorSpan._1))
    val updated = reducedDuplicates ::: noDuplicates
    updated.sortWith((one, other) => {
      if (one.errorSpan._1 != other.errorSpan._1) one.errorSpan._1 > other.errorSpan._1
      else one.correction.compareTo(other.correction) <= 0
    })
  }
  /*
    IMPORTANT-ASSUMPTION: All the corrections in the list refer to the same paragraph, like correctedParagraph method
   */
  private def reduceDuplicates(list: List[ConllCorrection]): ConllCorrection =
    list match {
      case List(cor1, cor2) => {
        val start_off = if (cor1.errorSpan._2 >= cor2.errorSpan._2)  cor1.errorSpan._2  else cor2.errorSpan._2
        val correction = {
          if (!cor1.correction.isEmpty && cor2.correction.isEmpty)
            cor1.correction
          else if (!cor2.correction.isEmpty && cor1.correction.isEmpty)
            cor2.correction
          else if (cor2 == cor1)
            cor1.correction
          else throw new NullPointerException(s"Both of them contain corrections $list")
        }
        ConllCorrection(cor1.paragraph, (cor1.errorSpan._1, start_off), correction)
      }
      case _ => throw new IllegalArgumentException(s"More than expexted corrections: $list")
    }
}
