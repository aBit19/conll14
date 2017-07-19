package conll.util

import conll.model.ConllCorrection

object CorrectionCleaner {
  def clean(corrections: List[ConllCorrection]): List[ConllCorrection] = {
    if (corrections == null) throw new NullPointerException
    val cleanCorrections = handleDuplicates(corrections)
    sort(cleanCorrections)
  }

  private[conll] def handleDuplicates(conllCorrections: List[ConllCorrection]): List[ConllCorrection] = {
    val reducedDuplicates: List[ConllCorrection] = conllCorrections
      .groupBy(_.errorSpan._1)
      .iterator
      .filter(_._2.size > 1)
      .map(pair => reduceDuplicate(pair._2)).toList

    val duplicateIndex = reducedDuplicates.map(_.errorSpan._1).toSet
    val noDuplicates = conllCorrections.filter(corr => !duplicateIndex.contains(corr.errorSpan._1))
    reducedDuplicates ::: noDuplicates.filterNot(_.correction.isEmpty)
  }

  //All corrections are duplicates and refer to the same paragraph
  private def reduceDuplicate(duplicate: List[ConllCorrection]): ConllCorrection =
    duplicate match {
      case List(cor1, cor2) => {
        val start_off = if (cor1.errorSpan._2 >= cor2.errorSpan._2)  cor1.errorSpan._2  else cor2.errorSpan._2
        val correction = {
          if (!cor1.correction.isEmpty && cor2.correction.isEmpty)
            cor1.correction
          else if (!cor2.correction.isEmpty && cor1.correction.isEmpty)
            cor2.correction
          else if (cor2 == cor1)
            cor1.correction
          else  {
            if (cor1.errorSpan._2 >= cor2.errorSpan._2) cor1.correction else cor2.correction
          }
        }
        ConllCorrection(cor1.paragraph, (cor1.errorSpan._1, start_off), correction)
      }
      case _ => throw new IllegalArgumentException(s"More than expected corrections: $duplicate")
    }
  private def sort(cleanCorrections: List[ConllCorrection]): List[ConllCorrection] = {
    cleanCorrections.sortWith((one, other) => {
      if (one.errorSpan._1 != other.errorSpan._1) one.errorSpan._1 > other.errorSpan._1
      else one.correction.compareTo(other.correction) <= 0
    })
  }
}
