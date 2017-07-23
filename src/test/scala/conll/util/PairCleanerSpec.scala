package conll.util

import org.scalatest.{FlatSpec, Matchers}

class PairCleanerSpec extends FlatSpec with Matchers {

  "A PairCleaner" should "accept a training pair (input text, target text)" in {
    val pairCleaner = PairCleaner(("One sentence. Another One", "Target Sentence"))
  }

  it should "throw an Illegal Argument Exception in case the pair is null" in {
    an [IllegalArgumentException] should be thrownBy {
      PairCleaner(null)
    }
  }

  it can "accept a predicate (String, String) => Boolean" in {
    PairCleaner(("One Sentence.", "Another one"), (_, _) => true)
  }

  it should "throw an Illegal Argument Exception in case the predicate is null" in {
    an [IllegalArgumentException] should be thrownBy {
      PairCleaner(("", ""), null)
    }
  }

  it can "accept a distance limit which all pair should not pass in order to get printed" in {
    PairCleaner(("One Sentence", "Another"), editDistanceLimit = 10)
  }

  it should ", given training pair when clean() then return Option[(List[String], List[String])] representing the " +
    "cleaned sentences" in {
    val cleaner: Option[(List[String], List[String])] = PairCleaner(("One Sentence.", "Another one")).clean()
  }

  it should "return the cleaned texts not containing sentences that fail the given predicate" in {
    val cleaner = PairCleaner(("One Sentence. Second Sentence.", "One Sentencce. Second."),
      (s1, s2) => math.abs(s1.length - s2.length) < 3 ).clean()
    cleaner shouldEqual Some(List("One Sentence."), List("One Sentencce."))
  }

  it should "return None when clean and all of the pairs fail the predicate" in {
    val cleaner = PairCleaner(("None of. .", "One Sentencce. Second."),
      (s1, s2) => math.abs(s1.length - s2.length) < 3).clean()
    cleaner shouldEqual None
  }

  it should "not print pairs exceeding the edit distance limit printed" in {
    val  cleaner = PairCleaner(("One Sentence. Sentence. Third.", "Another one. Sentenc. Thied"),
      editDistanceLimit = 1).clean()
    cleaner shouldEqual Some((List("Sentence.", "Third."), List("Sentenc.", "Thied.")))
  }

}
