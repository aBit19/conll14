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
    val pairs: List[(NodeSeq, NodeSeq)] = loadEssayCorrectionsPairs()
    pairs map toPreprocessedEssay
  }

  private[conll] def loadEssayCorrectionsPairs(): List[(NodeSeq, NodeSeq)] = extractFrom(xmlEntries)(essayCorrectionsPair)
  private[conll] def xmlEntries = loadXMLFile() \\ "DOC"
  private[conll] def loadXMLFile(): xml.NodeSeq = loadFile(filepath)
  private[conll] def essayCorrectionsPair(node: xml.Node): (NodeSeq, NodeSeq) =  (node \\ "P", node \\ "MISTAKE")

  private[conll] def toPreprocessedEssay(pair: (NodeSeq, NodeSeq)): Either[ErrorMessage, PreprocessedEssay] =
    try {
      Right(makePreprocessedEssayFrom(pair))
    } catch {
      case ex: Exception => Left(ex.getMessage)
    }

  private[conll] def makePreprocessedEssayFrom(paragraphsCorrections: (NodeSeq, NodeSeq)): PreprocessedEssay = {
    val (paragraphsXML, correctionsXML) = paragraphsCorrections
    val (paragraphs, corrections) = (extractParagraphsFrom(paragraphsXML), extractConllCorrectionsFrom(correctionsXML))
    val paragraphIdxToCorrection: Map[Int, List[ConllCorrection]] = corrections.groupBy(_.paragraph)
    val paragraphToItsIndex: List[(Paragraph, Int)] = paragraphs.zipWithIndex
    val pairs: List[(Paragraph, List[ConllCorrection])] = paragraphToItsIndex
      .map(p => (p._1, cleanCorrections(paragraphIdxToCorrection.getOrElse(p._2, List.empty))))
    PreprocessedEssay(pairs)
  }
}

object ConllParser {

  def apply(filepath: String): ConllParser = new ConllParser(filepath)

  private[conll] def extractParagraphsFrom(nodeSeq: NodeSeq): List[Paragraph] =
    extractFrom(nodeSeq)(_.text.replaceAll("\n", ""))
  private[conll] def extractConllCorrectionsFrom(nodeSeq: NodeSeq): List[ConllCorrection] = extractFrom(nodeSeq)(corrections)
  private def corrections(node: xml.Node): ConllCorrection = {
    val (start, end)  = ((node \ "@start_par").text.toInt, (node \ "@end_par").text.toInt)
    val correction = node  \ "CORRECTION"
    val (start_off, end_off)  = ((node \ "@start_off").text.toInt, (node \ "@end_off").text.toInt)
    if (start != end && end_off != 0) throw new IllegalArgumentException(s"Span in two paragraphs: $start - $end: at ($start_off $end_off): $correction")
    ConllCorrection(start, (start_off, end_off), if (correction.isEmpty) " " else correction.text )
  }

  private def cleanCorrections(corrections: List[ConllCorrection]): List[ConllCorrection] = {
    val withoutDuplicates = handleDuplicates(corrections)
    withoutDuplicates.sortWith((one, other) => {
      if (one.errorSpan._1 != other.errorSpan._1) one.errorSpan._1 > other.errorSpan._1
      else one.correction.compareTo(other.correction) <= 0
    })
  }

  private[conll] def handleDuplicates(conllCorrections: List[ConllCorrection]): List[ConllCorrection] = {
    val reducedDuplicates: List[ConllCorrection] = conllCorrections
      .groupBy(_.errorSpan._1)
      .iterator
      .filter(_._2.size > 1)
      .map(pair => reduceDuplicate(pair._2)).toList

    val duplicateIndex = reducedDuplicates.map(_.errorSpan._1).toSet
    val noDuplicates = conllCorrections.filter(corr => !duplicateIndex.contains(corr.errorSpan._1))
    reducedDuplicates ::: noDuplicates
  }

  //All corrections are duplicates and refer to the same paragraph
  private def reduceDuplicate(duplicate: List[ConllCorrection]): ConllCorrection =
    duplicate match {
      case List(cor1, cor2) => {
        val start_off = if (cor1.errorSpan._2 >= cor2.errorSpan._2)  cor1.errorSpan._2  else cor2.errorSpan._2
        val correction = {
          if (!cor1.correction.isEmpty && cor2.correction.isEmpty)
            cor1.correction
          else if (!cor2.correction.isEmpty && cor1.correction.isEmpty)
            cor2.correction
          else if (cor2 == cor1)
            cor1.correction
          else throw new NullPointerException(s"Both of them contain corrections $duplicate")
        }
        ConllCorrection(cor1.paragraph, (cor1.errorSpan._1, start_off), correction)
      }
      case _ => throw new IllegalArgumentException(s"More than expected corrections: $duplicate")
    }

  private def extractFrom[T](nodeSeq: NodeSeq)(f: xml.Node => T): List[T] =
    (List.empty[T] /: nodeSeq) ((list, node) => f(node) :: list).reverse
}