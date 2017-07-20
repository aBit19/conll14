package conll.writer

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import conll.util.Constants.TEST_PATH

class TrainingSetWriterSpec extends FlatSpec with  Matchers with BeforeAndAfterEach {
val inputFile = new File(TEST_PATH + "input")
  val targetFile = new File(TEST_PATH + "target")

  "A TrainingSetWriter" should  "accept a filename and a (input text, target text)" in {
    val writer = TrainingSetWriter("test.txt", ("input", "target"))
  }

  it should "throw an IAException when the filename is null" in {
    an [IllegalArgumentException] should be thrownBy {
      TrainingSetWriter(null, ("input", "input"))
    }
  }

  it should "throw an IAE when the training pair is null" in {
    an [IllegalArgumentException] should be thrownBy {
      TrainingSetWriter("test.txt", null)
    }
  }

  it should "create two files: 1) {filename}.input and 2) {filename}.target containing the input text and the target" +
    "text respectively when print()" in {
    val (fileName, inputFile, targetFile) = makeFilenameAndFiles("1")
    TrainingSetWriter(fileName, ("input", "target")).print()
    assert(inputFile.isFile)
    assert(targetFile.isFile)

    //clean
    deleteFiles(inputFile, targetFile)
  }

  it should "throw an IAE in case either of the newly created files exist" in {
    an [IllegalArgumentException] should be thrownBy {
      val file = makeFilename("fail")
      TrainingSetWriter(file, ("input", "target")).print()
    }
  }

  it should "write the contents of the input text to the {givenFile}.input each sentence having a new line" in {
    val (filename, inputFile, targetFile) = makeFilenameAndFiles("3")
    TrainingSetWriter(filename, ("First Sentence. Second one.", "")).print()
    "First Sentence.\nSecond one." shouldEqual readFileContents(inputFile)

    deleteFiles(inputFile, targetFile)
  }


  private def makeFilenameAndFiles(suffix: String): (String, File, File) = {
    val file = makeFilename(suffix)
    (file, makeFile(s"${file}.input"), makeFile(s"${file}.target"))
  }
  private def makeFilename(testNum: String): String = s"${TEST_PATH}test_$testNum"
  private def makeFile(file: String): File = new File(file)
  private def deleteFiles(files: File*) = files.foreach(_.delete())
  private def readFileContents(file: File): String = scala.io.Source.fromFile(file).getLines().mkString("\n")

}
