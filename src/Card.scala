package com.namingbe.fsrs

import java.time.Instant
import java.util.UUID


case class Card(
  cardId: String = UUID.randomUUID().toString,
  state: Card.State = Card.State.Learning,
  private val maybeStep: Option[Int],
  stability: Option[Double] = None,
  difficulty: Option[Double] = None,
  due: Instant = Instant.now(),
  lastReview: Option[Instant] = None
) {
  val step: Option[Int] = maybeStep.orElse(Some(0).filter(_ => state == Card.State.Learning))

  def asString: String = ???
}

object Card {
  def fromString(s: String): Card = ???

  enum State:
    case Learning, Review, Relearning
}