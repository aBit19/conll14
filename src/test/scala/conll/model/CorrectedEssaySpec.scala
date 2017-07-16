package conll.model

import org.scalatest.{FlatSpec, Matchers}

class CorrectedEssaySpec extends FlatSpec with Matchers {

  "A CorrectedEssaySpec" should "accept a list of Paragraphs" in {
    CorrectedEssay(List.empty[Paragraph])
  }

  it should "store the passed paragraphs" in {
    val list = List("First paragraph")
    CorrectedEssay(list).list shouldEqual list
  }
}
