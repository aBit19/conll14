package conll.parser


import org.scalatest.{ FlatSpec, Matchers}
import java.io.File

import conll.model.{ConllCorrection, ErrorMessage, PreprocessedEssay}

import scala.xml.NodeSeq
class ConllParserSpec extends FlatSpec with Matchers {

  private val testfile = new File(".").getAbsolutePath + "/resources/test_data/test.sgml.xml"
  private val parser = ConllParser(testfile)

  "A ConllParser" should "accept the file containing the conll training set" in {
    ConllParser(testfile)
  }

  it should "throw FileNotFoundException if file not found" in {
    a [FileNotFoundException] should be thrownBy {
      ConllParser("file.invalid")
    }
  }

  it should "be able to parse the file and return an Either[ErrorMessage, List[PreprocessedEssay]] where the list" +
    " represent the file content and the ErrorMessage any potential error" in {
    val preprocessedEssays: Either[ErrorMessage, List[PreprocessedEssay]] = parser.parse()
  }

  it should "return an xml.Elem representing the content of the file" in {
    val xmlFile: xml.NodeSeq = parser.getXMLFile()
  }

  it should "return an xml.NodeSeq representing the entries of the file when getEntries()" in {
    val entries: xml.NodeSeq = parser.getEntries()
  }

  it should "return 1 entry given testfile" in {
    parser.getEntries().size should be (1)
  }

  it should "return a List[(paragraphs: xml.NodeSeq, corrections: xml.NodeSeq)] when getPairs()" in {
    val essays: List[(NodeSeq, NodeSeq)] = parser.getPairs()
  }

  it should "return 4 paragraphs for first essay given testfile" in {
    val (paragraphs, _) = parser.getPairs().head
    paragraphs.size should be (4)
  }

  it should "return 97 correction for the first essay" in {
    val (_, corrections) = parser.getPairs().head
    corrections.size should be (97)
  }

  it should "return a PreprocessedEssay given a (NodeSeq, NodeSeq)" in {
    val essay: (NodeSeq, NodeSeq) = parser.getPairs().head
    val preprocessedEssay: PreprocessedEssay = parser.makePreprocessedEssay(essay)
  }
}
