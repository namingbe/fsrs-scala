package com.namingbe.fsrs

import java.time.{Duration, Instant}

class Scheduler(
  parameters: SchedulerParameters = SchedulerParameters.DefaultParameters,
  desiredRetention: Double = 0.9,
  learningSteps: Vector[Duration] = Vector(Duration.ofMinutes(1), Duration.ofMinutes(10)),
  relearningSteps: Vector[Duration] = Vector(Duration.ofMinutes(10)),
  maximumInterval: Long = 36500,
  enableFuzzing: Boolean = true
) {
  private val decay: Double = -parameters.decayRate
  private val factor: Double = math.pow(0.9, 1.0 / decay) - 1

  def getCardRetrievability(card: Card, at: Instant = Instant.now()): Double = {
    card.lastReview.fold(0.0) { last =>
      val elapsedDays = math.max(0, Duration.between(last, at).toDays)
      math.pow(1 + factor * elapsedDays / card.stability.get, decay)
    }
  }

  def reviewCard(
    card: Card,
    rating: ReviewLog.Rating,
    reviewDateTime: Option[Instant] = None,
    reviewDuration: Option[Long] = None
  ): (Card, ReviewLog) = ???

  def validateParameters(): Unit = ???

  private def initialStability(rating: ReviewLog.Rating): Double = ???
  private def initialDifficulty(rating: ReviewLog.Rating): Double = ???
  private def nextInterval(stability: Double): Long = ???
  private def shortTermStability(stability: Double, rating: ReviewLog.Rating): Double = ???
  private def nextDifficulty(difficulty: Double, rating: ReviewLog.Rating): Double = ???
  private def nextStability(difficulty: Double, stability: Double, retrievability: Double, rating: ReviewLog.Rating): Double = ???
  private def nextForgetStability(difficulty: Double, stability: Double, retrievability: Double): Double = ???
  private def nextRecallStability(difficulty: Double, stability: Double, retrievability: Double, rating: ReviewLog.Rating): Double = ???
  private def getFuzzedInterval(interval: Duration): Duration = ???
  private def clampDifficulty(difficulty: Double): Double = ???
  private def clampStability(stability: Double): Double = ???
}

object Scheduler {
  val MinDifficulty: Double = 1.0
  val MaxDifficulty: Double = 10.0

  final case class FuzzRange(start: Double, end: Double, factor: Double)

  val FuzzRanges: Vector[FuzzRange] = Vector(
    FuzzRange(2.5, 7.0, 0.15),
    FuzzRange(7.0, 20.0, 0.1),
    FuzzRange(20.0, Double.PositiveInfinity, 0.05)
  )
}