package conll.clients

import conll.parser.ConllParser
import conll.util.{Constants, PairCleaner}
import conll.writer.TrainingSetWriter

object IntegrationEndToEnd extends App {
  val parser = ConllParser(Constants.CONLL_PATH)
  val preprocessedEssays = parser.parse()
  val essays: List[(List[String], List[String])] =
    for {
    optionalPrepr <- preprocessedEssays
    preprocessed <- optionalPrepr.toOption
    pair <- preprocessed.applyCorrections
    cleanedPair <- PairCleaner(pair,
      sentenceFilter = (s1, s2) => math.abs(s1.length - s2.length) < 9,
      editDistanceLimit = 20).clean()
  }  yield cleanedPair

  val writer = TrainingSetWriter("conll", essays)
  writer.print()
}
