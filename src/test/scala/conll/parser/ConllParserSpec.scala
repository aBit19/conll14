package conll.parser


import org.scalatest.{ FlatSpec, Matchers}
import java.io.File

import conll.model.{ErrorMessage, PreprocessedEssay}

import scala.xml.NodeSeq
class ConllParserSpec extends FlatSpec with Matchers {

  private val path = new File(".").getAbsolutePath + "/resources/test_data/"
  private val testFile1 = path + "test.sgml.xml"
  private val testFile2 = path + "test_2.sgml.xml"
  private val parser = ConllParser(testFile1)

  "A ConllParser" should "accept the file containing the conll training set" in {
    ConllParser(testFile1)
  }

  it should "throw IAE if file not found or it is not file" in {
    a [IllegalArgumentException] should be thrownBy {
      ConllParser("file.invalid")
    }
  }

  it should "be able to parse the file and return a List of Either[ErrorMessage, List[PreprocessedEssay]] where the " +
    "list represent the file content and the ErrorMessage any potential error" in {
    val preprocessedEssays: List[Either[ErrorMessage, PreprocessedEssay]] = parser.parse()
  }

  it should "return an xml.Elem representing the contents of the file when loadXMLFile()" in {
    val xmlFile: xml.NodeSeq = parser.loadXMLFile()
  }

  it should "return an xml.NodeSeq representing the entries of the file when loadEntries()" in {
    val entries: xml.NodeSeq = parser.essayEntries
  }

  it should "return 1 entry given testFile1" in {
    parser.essayEntries.size should be (1)
  }

  it should "return a List[(paragraphs: xml.NodeSeq, corrections: xml.NodeSeq)] when loadPairs()" in {
    val essays: List[(NodeSeq, NodeSeq)] = parser.loadEssayCorrectionPairs()
  }

  it should "return 4 paragraphs for first essay given testFile1" in {
    val (paragraphs, _) = parser.loadEssayCorrectionPairs().head
    paragraphs should have size 4
  }

  it should "return 97 correction for the first essay" in {
    val (_, corrections) = parser.loadEssayCorrectionPairs().head
    corrections should have size 97
  }

  it should "return a PreprocessedEssay given a (NodeSeq, NodeSeq)" in {
    val essay: (NodeSeq, NodeSeq) = parser.loadEssayCorrectionPairs().head
    val preprocessedEssay: PreprocessedEssay = parser.makePreprocessedEssay(essay)
  }

  it should "return 1 PreprocessedEssay given testFile1" in {
    val preprocessedEssays = parser.parse()
    preprocessedEssays should have length 1
  }

  it should "return a size of two PreprocessedEssays given testFile2" in {
    val preprocessed = ConllParser(testFile2).parse()
    preprocessed should have size 2
  }

  it should "return a List(Right, Left) when parse() given testFile2" in {}
  val preprocessed = ConllParser(testFile2).parse()
  assert(preprocessed match {
    case List(Right(_), Left(_)) => true
    case _ => false
  })
}
