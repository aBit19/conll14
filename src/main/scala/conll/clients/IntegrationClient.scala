package conll.clients

import conll.parser.ConllParser
import conll.util.Constants
object IntegrationClient extends App {
  val parser = ConllParser(Constants.CONLL_PATH)
  val essays = parser.parse()

  val corrected = essays
    .filter(_.isRight).map(_.toOption.get.applyCorrections.get._2)

  print(corrected.size)
}