package conll.clients

import conll.model.Corrector
import conll.util.Constants

object ApplyCorrectionsClient extends App {

  val essay = Constants.getConllParser1.parse() match {case List(Right(x)) => x}
  val corrector = Corrector(essay)
  val (pre, corrected) = corrector.applyCorrections
  print(corrected.getOrElse("").split('.').toList.mkString("\n"))
}

