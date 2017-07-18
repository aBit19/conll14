package conll.clients

import conll.parser.ConllParser

object ErroredEssay extends App {
  val path = conll.util.Constants.path + "/errored6.sgml.xml"
  val parser = ConllParser(path)
  val preprocessedEssay = parser.parse().map(_.getOrElse(null)).head.applyCorrections
  print(preprocessedEssay.get._2.split('.').mkString("\n"))
}
