package conll.model

case class ConllCorrection(paragraph: Int, errorSpan: Span, correction: String) extends Ordered[ConllCorrection] {
  require(paragraph >= 0 &&
    (errorSpan._1 >= 0 && errorSpan._2 >= 0)
    && (errorSpan._1 < errorSpan._2)
    && correction != null)

  override def compare(that: ConllCorrection): Int =
    if (paragraph != that.paragraph)
      paragraph - that.paragraph
    else if (errorSpan._1 != that.errorSpan._1)
      errorSpan._1 - that.errorSpan._1
    else errorSpan._2 - errorSpan._2
}

