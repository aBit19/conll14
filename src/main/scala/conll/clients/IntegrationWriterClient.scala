package conll.clients

import java.io.File

import conll.util.{Constants, EssayCorrector}
import conll.writer.TrainingSetWriter

import scala.io.StdIn

object IntegrationWriterClient extends App {
  val essay = Constants.getPreprocessedEssay
  val (input, target) = EssayCorrector(essay).applyCorrections
}
