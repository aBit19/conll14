package conll.writer

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import service.SequenceAlignmentService.editDistance
import TrainingSetWriter._

class TrainingSetWriter private(filename: String, trainingPair: (String, String),
                                printIfDifferentSize: Boolean, distanceThreshold: Int) {
  require(filename != null && trainingPair != null)

  private val inputFilename = s"$filename.input"
  private val targetFilename = s"$filename.target"
  private val isDistanceThresholdDefined = distanceThreshold > 0
  var inputTargetSentencefilter: (String, String) => Boolean = {case (x, y) => true}
  private val (inputSentences, targetSentences): (List[String], List[String]) =
    (extractSentences(trainingPair._1), extractSentences(trainingPair._2))
  private val pairsHaveDifferentNumberOfSentences = inputSentences.size != targetSentences.size

  def print(): Unit = {
    if (exist(filename))
      throw new IllegalArgumentException(s"Files already exist $filename.input/target")
    else if (!printIfDifferentSize && pairsHaveDifferentNumberOfSentences) return
    else writeFileContents()
  }

  private def writeFileContents(): Unit = {
    val (inputFiltered, targetFiltered) = filterSentences()
    writeTo(inputFilename, inputFiltered)
    writeTo(targetFilename, targetFiltered)
  }

  private def filterSentences(): (String, String) = {
    val sentenceFilter: (String, String) => Boolean = if (isDistanceThresholdDefined) (s1, s2) => inputTargetSentencefilter(s1, s2) &&
      editDistance.align(s1, s2) < distanceThreshold else inputTargetSentencefilter
    val (input, target) = TrainingSetWriter.filterSentences(inputSentences, targetSentences)(sentenceFilter)
    (TrainingSetWriter.makeStringFromList(input), makeStringFromList(target))
  }

  private def exist(filename: String): Boolean = new File(filename + ".input").exists() ||
    new File(filename +".target").exists()

}

object TrainingSetWriter {
  def apply(filename: String, trainingPair: (String, String),
            printIfDifferentSize: Boolean = true, distanceThreshold: Int = -1):
  TrainingSetWriter = new TrainingSetWriter(filename, trainingPair, printIfDifferentSize, distanceThreshold)

  private def writeTo(filename: String, fileContent: String): Unit = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(new File(filename))))
    try {
      out.write(fileContent)
    } finally {out.close()}
  }
  private def extractSentences(text: String): List[String] = text.split('.').toList.map(_.trim)
  private def makeStringFromList(sentences: List[String]) = {
    sentences.mkString(".\n") + "."
  }

  private def filterSentences(inputSentences: List[String], targetSentences: List[String])
                             (filter: (String, String) => Boolean): (List[String], List[String]) = {
    inputSentences.zip(targetSentences).filter(p => filter(p._1, p._2)).unzip
  }
}
