package conll.parser

object ClientParser extends App {
  import java.io.File

  val testFile = new File(".").getAbsolutePath + "/resources/test_data/test.sgml.xml"
  val corrections = ConllParser(testFile).parse() match  {
    case Right(x) => x.head.corrections
    case _ => List.empty
  }
}
