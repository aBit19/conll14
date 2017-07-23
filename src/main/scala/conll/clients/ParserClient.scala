package conll.clients

import conll.parser.ConllParser

object ParserClient extends App {
  import java.io.File
  val testFile = new File(".").getAbsolutePath + "/resources/test_data/test.sgml.xml"
  val essay = ConllParser(testFile).parse() match {
    case List(Right(x)) => x
  }
  print()
}
