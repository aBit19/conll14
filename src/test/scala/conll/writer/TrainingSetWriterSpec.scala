package conll.writer

import java.io.File

import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}
import conll.util.Constants.TEST_PATH

class TrainingSetWriterSpec extends FlatSpec with  Matchers with BeforeAndAfterEach {

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
    " text respectively when print()" in {
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
    TrainingSetWriter(filename, ("First Sentence. Second one.", "First. Second.")).print()
    "First Sentence.\nSecond one." shouldEqual readFileContents(inputFile)

    deleteFiles(inputFile, targetFile)
  }

  it should "write the contents of the target text to the {givenFile}.target each sentence having a new line" in {
    val (filename, inputFile, targetFile) =  makeFilenameAndFiles("4")
    TrainingSetWriter(filename, ("First. Second.", "First Sentence. Second one.")).print()
    "First Sentence.\nSecond one." shouldEqual readFileContents(targetFile)

    deleteFiles(inputFile, targetFile)
  }

  it should "accept a predicate whether or not to print the essay pair in case the number of sentences are not equal the" +
    " default is true (it will print the pair)" in {
    val (filename, inputFile, targetFile) = makeFilenameAndFiles("5")
    TrainingSetWriter(filename, ("First Sentence.", "First Sentence. Second Sentence"), printIfDifferentSize = false).print()
    assert(!inputFile.exists())
    assert(!targetFile.exists())
  }
  it should "given two texts (input, target) having same number of sentences when printIfDifferentSize is false" +
    " and print then the files must exist containing the contents of each essay" in {
    val (filename, inputFile, targetFile) = makeFilenameAndFiles("6")
    TrainingSetWriter(filename, ("First Sentence. Second one.", "First Sentence. Second Sentence."),
      printIfDifferentSize = false).print()
    readFileContents(inputFile) shouldEqual "First Sentence.\nSecond one."
    readFileContents(targetFile) shouldEqual "First Sentence.\nSecond Sentence."

    deleteFiles(inputFile, targetFile)
  }

  it should "accept an edit distance threshold where each input-target sentence pair should not pass in order to get " +
    " printed" in {
    val (filename, inputFile, targetFile) = makeFilenameAndFiles("7")
    TrainingSetWriter(filename, ("Second Sentence.", "Second One"), distanceThreshold = 10)
  }

  it should "not print sentence pairs having more than the given distance." in {
    val (filename, inputFile, targetFile) = makeFilenameAndFiles("8")
    TrainingSetWriter(filename, ("Second Sentence. Another one", "Second Sentence. Another"), distanceThreshold = 1).print()
    readFileContents(inputFile) shouldEqual "Second Sentence."
    readFileContents(targetFile) shouldEqual "Second Sentence."

    deleteFiles(inputFile, targetFile)
  }

  it should "be able to accept a predicate taking two strings representing the input and target sentence" in {
    val (filename, expectedInputFile, expectedTargetFile) = makeFilenameAndFiles("9")
    val writer = TrainingSetWriter(filename, ("Second Sentence. Another one", "Second Sentence. Another"),
      distanceThreshold = 1)
    writer.inputTargetSentencefilter = (_, _) => true
  }

  it should "be able to apply the given predicate" in {
    val (filename, inputFileExpected, targetFileExpected) = makeFilenameAndFiles("9")
    val writer = TrainingSetWriter(filename,
      ("Second Sentence. This sentence should not printed", "Second Sentence. Another"))
    writer.inputTargetSentencefilter = (input, target) => math.abs(input.length - target.length) < 4
    writer.print()

    readFileContents(inputFileExpected) shouldEqual "Second Sentence."
    readFileContents(targetFileExpected) shouldEqual "Second Sentence."

    deleteFiles(inputFileExpected, targetFileExpected)
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
