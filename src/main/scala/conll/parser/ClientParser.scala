package conll.parser

object ClientParser extends App {
  import java.io.File
  val testFile = new File(".").getAbsolutePath + "/resources/test_data/test_2.sgml.xml"
  print(ConllParser(testFile).parse())
}
