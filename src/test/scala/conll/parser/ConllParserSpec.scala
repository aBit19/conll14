package conll.parser

import java.io.FileNotFoundException

import org.scalatest.{FlatSpec, Matchers}
import java.io.File

import conll.model.PreprocessedEssay
class ConllParserSpec extends FlatSpec with Matchers {

  private val testfile = new File(".").getAbsolutePath + "/resources/test_data/test.sgml.xml"
  "A ConllParser" should "accept the file containing the conll training set" in {
    ConllParser(new File(".").getAbsolutePath + "/src/test/scala/conll/model/PreprocessedEssaySpec.scala")
  }

  it should "throw FileNotFoundException if file not found" in {
    a [FileNotFoundException] should be thrownBy {
      ConllParser("file.invalid")
    }
  }

  it should "be able to parse the file and return an Either[Throwable, List[PreprocessedEssay]] where the list" +
    "represent the file content and the throwable any potential error" in {
    val parser = ConllParser(testfile)
    val preprocessedEssays: Either[Throwable, List[PreprocessedEssay]] = parser.parse()
  }
}
