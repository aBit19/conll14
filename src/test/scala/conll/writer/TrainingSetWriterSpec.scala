package conll.writer

import java.io.File

import conll.model.SentencePairs
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import conll.util.Constants.TEST_PATH

class TrainingSetWriterSpec extends FlatSpec with  Matchers with BeforeAndAfterEach {

  private val DUMMY_ESSAYS: List[SentencePairs] =
    List(
      (List("1st input 1st text.", "2nd input first text."), List("1st target 1st text.", "2nd target 1st text.")),
      (List("1st input 2nd text.", "2nd input 2nd text."), List("1st target 2nd text.", "2nd target 2nd text."))
    )

  private val inputContents = "1st input 1st text.\n2nd input first text.\n1st input 2nd text.\n2nd input 2nd text."
  private val targetContents = "1st target 1st text.\n2nd target 1st text.\n1st target 2nd text.\n2nd target 2nd text."

  "A TrainingSetWriter" should  "accept a filename and a List[OptionalSentencePair]" in {
    val writer = TrainingSetWriter("test.txt", DUMMY_ESSAYS)
  }

  it should "throw an IAException when the filename is null" in {
    an [IllegalArgumentException] should be thrownBy {
      TrainingSetWriter(null, DUMMY_ESSAYS)
    }
  }
  it should "throw an IAE when the training pair is null" in {
    an [IllegalArgumentException] should be thrownBy {
      TrainingSetWriter("test.txt", null)
    }
  }

  it should "create two files: 1) {filename}.input and 2) {filename}.target containing the input text and the target" +
    " text respectively when print()" in {
    val (fileName, inputFile, targetFile) = makeFilenameAndFiles("1")
    TrainingSetWriter(fileName, DUMMY_ESSAYS).print()
    assert(inputFile.isFile)
    assert(targetFile.isFile)

    //clean
    deleteFiles(inputFile, targetFile)
  }

  it should "throw an IAE in case either of the newly created files exist" in {
    an [IllegalArgumentException] should be thrownBy {
      val file = makeFilename("fail")
      TrainingSetWriter(file, DUMMY_ESSAYS).print()
    }
  }

  it should "write the contents of the input text to the {givenFile}.input each sentence having a new line" in {
    val (filename, inputFile, targetFile) = makeFilenameAndFiles("3")
    TrainingSetWriter(filename, DUMMY_ESSAYS).print()
    readFileContents(inputFile) shouldEqual inputContents

    deleteFiles(inputFile, targetFile)
  }
  it should "write the contents of the target text to the {givenFile}.target each sentence having a new line" in {
    val (filename, inputFile, targetFile) =  makeFilenameAndFiles("4")
    TrainingSetWriter(filename, DUMMY_ESSAYS).print()
    readFileContents(targetFile) shouldEqual targetContents

    deleteFiles(inputFile, targetFile)
  }

  private def makeFilenameAndFiles(suffix: String): (String, File, File) = {
    val fileName = makeFilename(suffix)
    (fileName, makeFile(s"$fileName.input"), makeFile(s"$fileName.target"))
  }
  private def makeFilename(testNum: String): String = s"${TEST_PATH}test_$testNum"
  private def makeFile(file: String): File = new File(file)
  private def deleteFiles(files: File*) = files.foreach(_.delete())
  private def readFileContents(file: File): String = scala.io.Source.fromFile(file).getLines().mkString("\n")
}
