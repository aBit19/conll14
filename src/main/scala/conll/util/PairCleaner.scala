package conll.util

import PairCleaner._
import service.SequenceAlignmentService.editDistance

class PairCleaner private (training: (String, String),
                           sentenceFilter: (String, String) => Boolean,
                           editDistanceLimit: Int) {
  require(training != null && sentenceFilter != null)

  private val isEditLimitDefined = editDistanceLimit >= 0

  def clean(): Option[(List[String], List[String])] = {
    val (inputSentences, targetSentences) = (extractSentences(training._1), extractSentences(training._2))
    val filteredPair = if (isEditLimitDefined)
      inputSentences.zip(targetSentences)
        .filter(pair =>
          sentenceFilter.tupled(pair)
            && (editDistance.align(pair._1, pair._2) <= editDistanceLimit)).unzip
    else inputSentences.zip(targetSentences).filter(sentenceFilter.tupled).unzip
    if (filteredPair._1.isEmpty) None
    else Some(filteredPair)
  }
}

object PairCleaner {
  def apply(training: (String, String),
            sentenceFilter: (String, String) => Boolean = (_, _) => true,
            editDistanceLimit: Int = -1): PairCleaner =
    new PairCleaner(training, sentenceFilter, editDistanceLimit)

  private def extractSentences(text: String): List[String] = {
    text.split('.').map(_.trim + " .").toList
  }

}
