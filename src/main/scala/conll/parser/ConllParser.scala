package conll.parser

import conll.model.{ConllCorrection, ErrorMessage, Paragraph, PreprocessedEssay}

import java.io.File
import scala.xml.NodeSeq
import xml.XML.loadFile
import ConllParser._

class ConllParser private (filepath: String) {
  require(new File(filepath).isFile)

  def parse(): List[Either[ErrorMessage, PreprocessedEssay]] = extractPreprocessedEssays()

  private[conll] def extractPreprocessedEssays() = {
    val pairs: List[(NodeSeq, NodeSeq)] = loadEssayCorrectionPairs()
    pairs map toPreprocessedEssay
  }

  private[conll] def loadEssayCorrectionPairs(): List[(NodeSeq, NodeSeq)] = extractFrom(essayEntries)(correctionEssayPair)
  private[conll] def essayEntries = loadXMLFile() \\ "DOC"
  private[conll] def loadXMLFile(): xml.NodeSeq = loadFile(filepath)
  private[conll] def correctionEssayPair(node: xml.Node): (NodeSeq, NodeSeq) =  (node \\ "P", node \\ "MISTAKE")

  private[conll] def toPreprocessedEssay(pair: (NodeSeq, NodeSeq)): Either[ErrorMessage, PreprocessedEssay] =
    try {
      Right(PreprocessedEssay(extractParagraphs(pair._1), extractCorrections(pair._2)))
    } catch {
      case ex: Exception => Left(ex.getMessage)
    }

  private[conll] def makePreprocessedEssay(paragraphsCorrections: (NodeSeq, NodeSeq)): PreprocessedEssay = {
    val (paragraphs, corrections) = paragraphsCorrections
    PreprocessedEssay(extractParagraphs(paragraphs), extractCorrections(corrections))
  }
}

object ConllParser {

  def apply(filepath: String): ConllParser = new ConllParser(filepath)

  private[conll] def extractParagraphs(nodeSeq: NodeSeq): List[Paragraph] =
    extractFrom(nodeSeq)(_.text.replaceAll("\n", ""))
  private[conll] def extractCorrections(nodeSeq: NodeSeq): List[ConllCorrection] = extractFrom(nodeSeq)(corrections)
  private def corrections(node: xml.Node): ConllCorrection = {
    val (start, end)  = ((node \ "@start_par").text.toInt, (node \ "@end_par").text.toInt)
    if (start != end) throw new IllegalArgumentException(s"Span in two paragraphs: $start - $end ")
    val (start_off, end_off)  = ((node \ "@start_off").text.toInt, (node \ "@end_off").text.toInt)
    val correction = node  \ "CORRECTION"
    ConllCorrection(start, (start_off, end_off), if (correction.isEmpty) " " else correction.text )
  }

  private def extractFrom[T](nodeSeq: NodeSeq)(f: xml.Node => T): List[T] =
    (List.empty[T] /: nodeSeq) ((list, node) => f(node) :: list).reverse
}