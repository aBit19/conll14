package conll.model

case class ConllCorrection(paragraph: Int)(errorSpan: Span)(correction: String) {
  require(paragraph >= 0 &&
    (errorSpan._1 >= 0 && errorSpan._2 >= 0)
    && (errorSpan._1 < errorSpan._2)
    && correction != null)
}

