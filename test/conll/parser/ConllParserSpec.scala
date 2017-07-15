package conll.parser

import org.scalatest.FlatSpec


class ConllParserSpec extends FlatSpec {
  "A ConllParser" should "accept a file name" in {
    val conllParser: ConllParser = new ConllParser("official.sgml.xml")
  }
}
