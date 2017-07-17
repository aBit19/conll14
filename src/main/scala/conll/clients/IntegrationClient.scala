package conll.clients

import conll.model.PreprocessedEssay
import conll.parser.ConllParser
import conll.util.Constants
object IntegrationClient extends App {
  val parser = ConllParser(Constants.conll)
  val list = parser.parse().filter(_.isRight).map(_.getOrElse(PreprocessedEssay(List.empty, List.empty)))
  print(list(8).applyCorrections._2)
}
