package conll.util

import conll.model.PreprocessedEssay
import conll.parser.ConllParser

object Constants {
  import java.io.File
  private val path = new File(".").getAbsolutePath + "/resources/test_data/"
  val testFile1: String = path + "/test.sgml.xml"
  val  testFile2: String = path + "/test_2.sgml.xml"
  def getConllParser1 = ConllParser(testFile1)
  def getConllParser2 = ConllParser(testFile2)
  val getPreprocessedEssay: PreprocessedEssay = getConllParser1.parse() match {case List(Right(x)) => x}
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
