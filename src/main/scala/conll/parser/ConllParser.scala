package conll.parser

import conll.model.PreprocessedEssay

import scala.io.Source
class ConllParser private (filepath: String){

  private val source = Source.fromFile(filepath)
  def parse(): Either[Throwable, List[PreprocessedEssay]] = Right(List.empty[PreprocessedEssay])
}

object ConllParser {
  def apply(filepath: String): ConllParser = new ConllParser(filepath)
}