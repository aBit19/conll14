package conll.clients

import java.io.File

import conll.util.{Constants, EssayCorrector}
import conll.writer.TrainingSetWriter

import scala.io.StdIn

object IntegrationWriterClient extends App {
  val essay = Constants.getPreprocessedEssay
  val (input, target) = EssayCorrector(essay).applyCorrections
  val writes = TrainingSetWriter("integration.conll", (input, target.getOrElse("")), distanceThreshold = 50)
  writes.inputTargetSentenceFilter = (input, target) => math.abs(input.length - target.length) < 12
  writes.print()
  StdIn.readLine()
  new File("integration.conll.input").delete()
  new File("integration.conll.target").delete()
}
