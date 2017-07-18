package conll.clients

import conll.parser.ConllParser
import conll.util.Constants
object IntegrationClient extends App {
  val parser = ConllParser(Constants.conll)
  val list = parser.parse().filter(_.isLeft)
  print(list.mkString("\n"))
}
