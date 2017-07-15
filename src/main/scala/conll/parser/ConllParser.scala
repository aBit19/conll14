package conll.parser

import conll.model.{ConllCorrection, ErrorMessage, Paragraph, PreprocessedEssay}

import scala.io.Source
import scala.xml.NodeSeq
import xml.XML.loadFile

class ConllParser private (filepath: String) {
  import ConllParser._
  private val source = Source.fromFile(filepath)
  def parse(): Either[ErrorMessage, List[PreprocessedEssay]] = {
    val pairs = getPairs()
    try {
      Right(pairs.map(makePreprocessedEssay))
    } catch {
      case ex: Exception => Left(ex.getMessage)
    }
  }

  private[conll] def getXMLFile(): xml.NodeSeq = loadFile(filepath)
  private[conll] def getEntries(): xml.NodeSeq = getXMLFile() \\ "DOC"
  private[conll] def getPairs(): List[(NodeSeq, NodeSeq)] = extractFrom(getEntries())(getPair)

  private[conll] def makePreprocessedEssay(paragraphsCorrections: (NodeSeq, NodeSeq)): PreprocessedEssay = {
    val (paragraphs, corrections) = paragraphsCorrections
    PreprocessedEssay(extractParagraphs(paragraphs), extractCorrections(corrections))
  }
}

object ConllParser {
  def apply(filepath: String): ConllParser = new ConllParser(filepath)
  private def getPair(node: xml.Node) =  (node \\ "P", node \\ "MISTAKE")
  private def extractParagraphs(nodeSeq: NodeSeq): List[Paragraph] = extractFrom(nodeSeq)(_.text)
  private def extractCorrections(nodeSeq: NodeSeq): List[ConllCorrection] = extractFrom(nodeSeq)(extractCorrection)
  private def extractFrom[T](nodeSeq: NodeSeq)(f: xml.Node => T): List[T] = (List.empty[T] /: nodeSeq) ((list, node) => f(node) :: list)

  private def extractCorrection(node: xml.Node): ConllCorrection = {
    val (start, end)  = ((node \ "@start_par").text.toInt, (node \ "@end_par").text.toInt)
    if (start != end) throw new IllegalArgumentException(s"Span in two paragraphs: $start - $end ")
    val (start_off, end_off)  = ((node \ "@start_off").text.toInt, (node \ "@end_off").text.toInt)
    if (start_off >= end_off) throw new InterruptedException(s"invalid start_off: $start_off - $end_off")
    val correction = node  \ "CORRECTION"
    ConllCorrection(start)((start_off, end_off))(if (correction.isEmpty) "" else correction.text )
  }
}