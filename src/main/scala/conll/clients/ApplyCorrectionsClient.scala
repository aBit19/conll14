package conll.clients

import conll.model.PreprocessedEssay
import conll.util.{Constants, EssayCorrector}

object ApplyCorrectionsClient extends App {

  val essay: PreprocessedEssay = Constants.getConllParser1.parse() match {case List(Right(x)) => x}
  val corrector = EssayCorrector(essay)
  val (pre, corrected) = corrector.applyCorrections
  print(corrected getOrElse "" split '.'  mkString "\n" )
}

