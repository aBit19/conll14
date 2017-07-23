package conll

package object model {
  type Paragraph = String
  type ErrorMessage = String
  type Span = (Int, Int)
  type SentencePairs = (List[String], List[String])
}
