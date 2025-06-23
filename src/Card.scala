package com.namingbe.fsrs

import java.time.Instant
import java.util.UUID


case class Card(
  cardId: String = UUID.randomUUID().toString,
  state: Card.State = Card.State.StepBased(0),
  stability: Option[Stability] = None,
  difficulty: Option[Difficulty] = None,
  due: Instant = Instant.now(),
  lastReview: Option[Instant] = None
)

object Card {
  enum State:
    case StepBased(step: Int)
    case Review
}