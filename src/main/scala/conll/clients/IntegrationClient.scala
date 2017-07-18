package conll.clients

import conll.model.Corrector
import conll.parser.ConllParser
import conll.util.Constants
object IntegrationClient extends App {
  val parser = ConllParser(Constants.conll)
  val essays = parser.parse()

  val corrected = essays
    .filter(_.isLeft)

  print(corrected.size)
}
