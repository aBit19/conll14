package conll.util

import conll.model.PreprocessedEssay
import conll.parser.ConllParser

object Constants {
  import java.io.File
  val TEST_PATH: String = new File(".").getAbsolutePath + "/resources/test_data/"
  val CONLL_PATH: String = new File(".").getAbsolutePath + "/resources/data_conll/official.sgml.xml"
  val TEST_FILE_1: String = TEST_PATH + "/test.sgml.xml"
  val  TEST_FILE_2: String = TEST_PATH + "/test_2.sgml.xml"
  def getConllParser1 = ConllParser(TEST_FILE_1)
  def getConllParser2 = ConllParser(TEST_FILE_2)
  lazy val getPreprocessedEssay: PreprocessedEssay = getConllParser1.parse() match {case List(Right(x)) => x}
  def sequentialForAll[T](list: List[T], test: (T, T) => Boolean): Boolean = {
    def helper(elem: T, rem: List[T]): Boolean = {
      rem match {
        case Nil => true
        case head :: tail if test(elem, head) => helper(head, tail)
        case _ => false
      }
    }

    if (list.isEmpty) true
    else {
      helper(list.head, list.tail)
    }
  }
}
