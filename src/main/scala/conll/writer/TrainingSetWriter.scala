package conll.writer

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import TrainingSetWriter._
import conll.model.SentencePairs

class TrainingSetWriter private(filename: String, sentencePairs: List[SentencePairs]) {
  require(filename != null && sentencePairs!= null)

  private val inputFilename = s"$filename.input"
  private val targetFilename = s"$filename.target"

  def print(): Unit = {
    if (filesExist())
      throw new IllegalArgumentException(s"Files already exist $filename.input/target")
    val (sentenceInputPairs, sentenceTargetPairs) = concatPairs
    val (inputContent, targetContent): (String, String) =
      (makeStringFrom(sentenceInputPairs), makeStringFrom(sentenceTargetPairs))
    writeTo(inputFilename, inputContent)
    writeTo(targetFilename, targetContent)
  }

  private def filesExist(): Boolean = new File(targetFilename).exists() ||
    new File(inputFilename).exists()

  private def concatPairs: (List[String], List[String]) = {
    val definedPairs = for {
      pair <- sentencePairs
    } yield pair
      definedPairs.reduce[(List[String], List[String])](
        (pair1, pair2) => (pair1._1 ++ pair2._1, pair1._2 ++ pair2._2))
  }

}

object TrainingSetWriter {
  def apply(filename: String, trainingPair: List[SentencePairs]):
  TrainingSetWriter = new TrainingSetWriter(filename, trainingPair)

  private def writeTo(filename: String, fileContent: String): Unit = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(new File(filename))))
    try {
      out.write(fileContent)
    } finally {out.close()}
  }
  private def makeStringFrom(sentences: List[String]): String = {
    sentences.mkString("\n")
  }
}
