package conll.clients

import conll.parser.ConllParser
import conll.util.Constants
object IntegrationClient extends App {
  val parser = ConllParser(Constants.conll)
  val essays = parser.parse()

  val corrected = essays
    .filter(_.isRight).map(_.toOption.get.applyCorrections)

  print(corrected.size)
}
