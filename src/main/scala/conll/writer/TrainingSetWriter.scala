package conll.writer

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

class TrainingSetWriter private(filename: String, trainingPair: (String, String)) {
  require(filename != null && trainingPair != null)

  private val inputFilename = s"$filename.input"
  private val targetFilename = s"$filename.target"
  def print(): Unit = {
    if (exist(filename))
      throw new IllegalArgumentException(s"Files already exist ${filename}.input/target")
    TrainingSetWriter.writeTo(inputFilename, trainingPair._1)
    TrainingSetWriter.writeTo(targetFilename, trainingPair._2)
  }

  private def exist(filename: String): Boolean = new File(filename + ".input").exists() ||
    new File(filename +".target").exists()
}

object TrainingSetWriter {
  def apply(filename: String, trainingPair: (String, String)): TrainingSetWriter =
    new TrainingSetWriter(filename, trainingPair)

  private def writeTo(filename: String, content: String): Unit = {
    val out = new PrintWriter(new BufferedWriter(new FileWriter(new File(filename))))
    try {
      val toPrint = content.split('.').map(_.trim).mkString(".\n") + "."
      out.write(toPrint)
    } finally {out.close()}
  }
}
