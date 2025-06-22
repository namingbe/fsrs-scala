package com.namingbe.fsrs

import java.time.Instant

case class ReviewLog(
  cardId: String,
  rating: ReviewLog.Rating,
  reviewDateTime: Instant,
  reviewDuration: Option[Long]
) {
  def asString: String = ???
}

object ReviewLog {
  def fromString(s: String): ReviewLog = ???

  enum Rating extends Ordered[Rating] {
    case Again, Hard, Good, Easy

    override def compare(that: Rating): Int = this.ordinal - that.ordinal

    def value: Int = ordinal + 1
  }

  object Rating {
    def fromValue(value: Int): Rating = Rating.fromOrdinal(value - 1)
  }
}