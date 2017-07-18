package conll.model

case class ConllCorrection(paragraph: Int, errorSpan: Span, correction: String) {
  if(!(paragraph >= 0 &&
    (errorSpan._1 >= 0 && errorSpan._2 >= 0)
    && correction != null)) throw new IllegalArgumentException(s"Correction($paragraph $errorSpan $correction)")
}

