package com.namingbe.fsrs

import Card.State
import ReviewLog.Rating
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
    rating: Rating,
    reviewedAt: Instant = Instant.now,
  ): (Card, ReviewLog) = {
    val isInitialLearning = card.stability.isEmpty
    val stepSequence = if (isInitialLearning) learningSteps else relearningSteps

    val (newStability, newDifficulty) = updateMemoryParameters(card, rating, reviewedAt)

    val (nextState, nextInterval) = card.state match {
      case State.StepBased(currentStep) => calculateStepBasedTransition(currentStep, stepSequence, rating, newStability)
      case State.Review => calculateReviewTransition(rating, newStability, stepSequence)
    }

    val finalInterval = if (enableFuzzing && nextState == State.Review) {
      getFuzzedInterval(nextInterval)
    } else {
      nextInterval
    }

    val finalCard = card.copy(
      state = nextState,
      stability = Some(newStability),
      difficulty = Some(newDifficulty),
      due = reviewedAt.plus(finalInterval),
      lastReview = Some(reviewedAt)
    )

    val reviewLog = ReviewLog(
      cardId = card.cardId,
      rating = rating,
      reviewedAt = reviewedAt,
    )

    (finalCard, reviewLog)
  }

  private def updateMemoryParameters(
    card: Card,
    rating: Rating,
    reviewedAt: Instant
  ): (Double, Double) = {
    if (card.stability.isEmpty && card.difficulty.isEmpty) {
      // First review - set initial values
      (initialStability(rating), initialDifficulty(rating))
    } else {
      val isSameDayReview = card.lastReview.exists { lastReview =>
        Duration.between(lastReview, reviewedAt).toDays < 1
      }

      if (isSameDayReview) {
        // Same-day review - use short-term stability formula
        val newStability = shortTermStability(card.stability.get, rating)
        val newDifficulty = nextDifficulty(card.difficulty.get, rating)
        (newStability, newDifficulty)
      } else {
        // Multi-day review - use full FSRS formulas
        val retrievability = getCardRetrievability(card, reviewedAt)
        val newStability = nextStability(card.difficulty.get, card.stability.get, retrievability, rating)
        val newDifficulty = nextDifficulty(card.difficulty.get, rating)
        (newStability, newDifficulty)
      }
    }
  }

  private def calculateStepBasedTransition(
    currentStep: Int,
    stepSequence: Vector[Duration],
    rating: Rating,
    stability: Double
  ): (State, Duration) = {

    // Check if we should graduate to Review state
    val shouldGraduate = stepSequence.isEmpty ||
      (currentStep >= stepSequence.length &&
        Set(Rating.Hard, Rating.Good, Rating.Easy).contains(rating))

    if (shouldGraduate) {
      (State.Review, Duration.ofDays(nextInterval(stability)))
    } else {
      // Stay in StepBased state with step-based intervals
      val (nextStep, stepInterval) = rating match {
        case Rating.Again =>
          (0, stepSequence(0))

        case Rating.Hard =>
          val interval = if (currentStep == 0 && stepSequence.length == 1) {
            stepSequence(0).multipliedBy(15).dividedBy(10) // * 1.5
          } else if (currentStep == 0 && stepSequence.length >= 2) {
            Duration.ofMillis((stepSequence(0).toMillis + stepSequence(1).toMillis) / 2)
          } else {
            stepSequence(currentStep)
          }
          (currentStep, interval)

        case Rating.Good =>
          if (currentStep + 1 == stepSequence.length) {
            // Graduate to Review state
            return (State.Review, Duration.ofDays(nextInterval(stability)))
          } else {
            (currentStep + 1, stepSequence(currentStep + 1))
          }

        case Rating.Easy =>
          // Graduate to Review state immediately
          return (State.Review, Duration.ofDays(nextInterval(stability)))
      }

      (State.StepBased(nextStep), stepInterval)
    }
  }

  private def calculateReviewTransition(
    rating: Rating,
    stability: Double,
    relearningSteps: Vector[Duration]
  ): (State, Duration) = {
    rating match {
      case Rating.Again =>
        if (relearningSteps.isEmpty) {
          (State.Review, Duration.ofDays(nextInterval(stability)))
        } else {
          (State.StepBased(0), relearningSteps(0))
        }

      case Rating.Hard | Rating.Good | Rating.Easy =>
        (State.Review, Duration.ofDays(nextInterval(stability)))
    }
  }

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