package conll.parser

import conll.model.{ConllCorrection, ErrorMessage, Paragraph, PreprocessedEssay}

import java.io.File
import scala.xml.NodeSeq
import xml.XML.loadFile

class ConllParser private (filepath: String) {
  require(new File(filepath).isFile)
  import ConllParser._

  def parse(): Either[ErrorMessage, List[PreprocessedEssay]] = {
    val pairs = loadEssayCorrectionPairs()
    try {
      Right(pairs.map(makePreprocessedEssay))
    } catch {
      case ex: Exception => Left(ex.getMessage)
    }
  }

  private[conll] def loadEssayCorrectionPairs(): List[(NodeSeq, NodeSeq)] = extractFrom(loadEntries())(getPair)
  private[conll] def loadEntries(): xml.NodeSeq = loadXMLFile() \\ "DOC"
  private[conll] def loadXMLFile(): xml.NodeSeq = loadFile(filepath)

  private[conll] def makePreprocessedEssay(paragraphsCorrections: (NodeSeq, NodeSeq)): PreprocessedEssay = {
    val (paragraphs, corrections) = paragraphsCorrections
    PreprocessedEssay(extractParagraphs(paragraphs), extractCorrections(corrections))
  }
}

object ConllParser {

  def apply(filepath: String): ConllParser = new ConllParser(filepath)
  private[conll] def getPair(node: xml.Node) =  (node \\ "P", node \\ "MISTAKE")
  private[conll] def extractParagraphs(nodeSeq: NodeSeq): List[Paragraph] = extractFrom(nodeSeq)(_.text)
  private[conll] def extractCorrections(nodeSeq: NodeSeq): List[ConllCorrection] = extractFrom(nodeSeq)(extractCorrection)
  private[conll] def extractFrom[T](nodeSeq: NodeSeq)(f: xml.Node => T): List[T] =
   (List.empty[T] /: nodeSeq) ((list, node) => f(node) :: list).reverse

  private def extractCorrection(node: xml.Node): ConllCorrection = {
    val (start, end)  = ((node \ "@start_par").text.toInt, (node \ "@end_par").text.toInt)
    if (start != end) throw new IllegalArgumentException(s"Span in two paragraphs: $start - $end ")
    val (start_off, end_off)  = ((node \ "@start_off").text.toInt, (node \ "@end_off").text.toInt)
    val correction = node  \ "CORRECTION"
    ConllCorrection(start, (start_off, end_off), if (correction.isEmpty) "" else correction.text )
  }
}