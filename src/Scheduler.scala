package com.namingbe.fsrs

import java.time.{Duration, Instant}

class Scheduler(
  parameters: Vector[Double] = Scheduler.DefaultParameters,
  desiredRetention: Double = 0.9,
  learningSteps: Vector[Duration] = Vector(Duration.ofMinutes(1), Duration.ofMinutes(10)),
  relearningSteps: Vector[Duration] = Vector(Duration.ofMinutes(10)),
  maximumInterval: Long = 36500,
  enableFuzzing: Boolean = true
) {
  def getCardRetrievability(card: Card, currentDateTime: Option[Instant] = None): Double = ???

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
  val DefaultParameters: Vector[Double] = Vector(
    0.2172, 1.1771, 3.2602, 16.1507, 7.0114, 0.57, 2.0966, 0.0069, 1.5261, 0.112,
    1.0178, 1.849, 0.1133, 0.3127, 2.2934, 0.2191, 3.0004, 0.7536, 0.3332, 0.1437, 0.2
  )

  val StabilityMin: Double = 0.001
  val MinDifficulty: Double = 1.0
  val MaxDifficulty: Double = 10.0

  val LowerBoundsParameters: Vector[Double] = Vector(
    StabilityMin, StabilityMin, StabilityMin, StabilityMin, 1.0, 0.001, 0.001, 0.001, 0.0, 0.0,
    0.001, 0.001, 0.001, 0.001, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.1
  )

  val InitialStabilityMax: Double = 100.0

  val UpperBoundsParameters: Vector[Double] = Vector(
    InitialStabilityMax, InitialStabilityMax, InitialStabilityMax, InitialStabilityMax, 10.0, 4.0, 4.0, 0.75, 4.5, 0.8,
    3.5, 5.0, 0.25, 0.9, 4.0, 1.0, 6.0, 2.0, 2.0, 0.8, 0.8
  )

  final case class FuzzRange(start: Double, end: Double, factor: Double)

  val FuzzRanges: Vector[FuzzRange] = Vector(
    FuzzRange(2.5, 7.0, 0.15),
    FuzzRange(7.0, 20.0, 0.1),
    FuzzRange(20.0, Double.PositiveInfinity, 0.05)
  )
}