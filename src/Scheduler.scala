package com.namingbe.fsrs

import Card.State
import ReviewLog.Rating
import java.time.{Duration, Instant}
import scala.math.*

class Scheduler(
  parameters: SchedulerParameters = SchedulerParameters.DefaultParameters,
  desiredRetention: Double = 0.9,
  learningSteps: Vector[Duration] = Vector(Duration.ofMinutes(1), Duration.ofMinutes(10)),
  relearningSteps: Vector[Duration] = Vector(Duration.ofMinutes(10)),
  maximumInterval: Long = 36500,
  enableFuzzing: Boolean = true,
) {
  import parameters._

  private val decay: Double = -decayRate
  private val factor: Double = pow(0.9, 1.0 / decay) - 1

  def getCardRetrievability(card: Card, at: Instant = Instant.now()): Double = {
    card.lastReview.fold(0.0) { last =>
      val elapsedDays = max(0, Duration.between(last, at).toDays)
      pow(1 + factor * elapsedDays / card.stability.get.value, decay)
    }
  }

  def reviewCard(
    card: Card,
    rating: Rating,
    reviewedAt: Instant = Instant.now,
  ): (Card, ReviewLog) = {
    val (newStability, newDifficulty) = updateMemoryParameters(card, rating, reviewedAt)
    val (nextState, nextInterval) = updateStateAndInterval(card, rating, newStability)
    val finalInterval = getFuzzedInterval(nextInterval, nextState)

    val finalCard = Card(
      cardId = card.cardId,
      state = nextState,
      stability = Some(newStability),
      difficulty = Some(newDifficulty),
      due = reviewedAt.plus(finalInterval),
      lastReview = Some(reviewedAt),
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
  ): (Stability, Difficulty) = {
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

  private def updateStateAndInterval(
    card: Card,
    rating: Rating,
    stability: Stability
  ): (State, Duration) = {
    val isInitialLearning = card.stability.isEmpty
    val stepSequence = if (isInitialLearning) learningSteps else relearningSteps

    card.state match {
      case State.StepBased(currentStep) => calculateStepBasedTransition(currentStep, stepSequence, rating, stability)
      case State.Review => calculateReviewTransition(rating, stability, stepSequence)
    }
  }

  private def calculateStepBasedTransition(
    currentStep: Int,
    stepSequence: Vector[Duration],
    rating: Rating,
    stability: Stability,
  ): (State, Duration) = {
    if (currentStep >= stepSequence.length && rating != Rating.Again) {
      // Graduating from step-based
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
    stability: Stability,
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

  private def initialStability(rating: Rating): Stability =
    rating match {
      case Rating.Again => initialAgainStability
      case Rating.Hard => initialHardStability
      case Rating.Good => initialGoodStability
      case Rating.Easy => initialEasyStability
    }

  private def initialDifficulty(rating: Rating): Difficulty =
    Difficulty(baseDifficulty - exp(difficultyScale * (rating.value - 1)) + 1)

  private def nextInterval(stability: Stability): Long = {
    val interval = (stability.value / factor) * (pow(desiredRetention, 1.0 / decay) - 1)
    val days = round(interval)
    min(max(days, 1), maximumInterval)
  }

  private def shortTermStability(stability: Stability, rating: Rating): Stability = {
    val stabilityIncrease = exp(shortTermFactor * (rating.value - 3 + ratingOffset)) *
      pow(stability.value, -stabilityDiminish)

    val clampedIncrease = if (Set(Rating.Good, Rating.Easy).contains(rating)) {
      max(stabilityIncrease, 1.0)
    } else {
      stabilityIncrease
    }

    val newStability = stability.value * clampedIncrease
    Stability(newStability)
  }

  private def nextDifficulty(difficulty: Difficulty, rating: Rating): Difficulty = {
    def linearDamping(deltaDifficulty: Double): Double = {
      (10.0 - difficulty.value) * deltaDifficulty / 9.0
    }

    def meanReversion(arg1: Difficulty, arg2: Double): Double = {
      meanReversionWeight * arg1.value + (1 - meanReversionWeight) * arg2
    }

    val arg1 = initialDifficulty(Rating.Easy)
    val deltaDifficulty = -(difficultyChangeRate * (rating.value - 3))
    val arg2 = difficulty.value + linearDamping(deltaDifficulty)

    val newDifficulty = meanReversion(arg1, arg2)
    Difficulty(newDifficulty)
  }

  private def nextStability(difficulty: Difficulty, stability: Stability, retrievability: Double, rating: Rating): Stability =
    rating match {
      case Rating.Again => nextForgetStability(difficulty, stability, retrievability)
      case _ => nextRecallStability(difficulty, stability, retrievability, rating)
    }

  private def nextForgetStability(difficulty: Difficulty, stability: Stability, retrievability: Double): Stability = {
    val shortTerm = stability.value / exp(shortTermFactor * ratingOffset)
    val longTerm = List(
      forgetFactor,
      pow(difficulty.value, -difficultyImpact),
      pow(stability.value + 1, stabilityGrowth) - 1,
      exp((1 - retrievability) * forgetRetrievability),
    ).product
    Stability(min(shortTerm, longTerm))
  }

  private def nextRecallStability(difficulty: Difficulty, stability: Stability, retrievability: Double, rating: Rating): Stability = {
    val ratingFactor = rating match {
      case Rating.Hard => hardPenalty
      case Rating.Easy => easyBonus
      case _ => 1.0
    }
    val compositeFactor = List(
      exp(recallFactor),
      11 - difficulty.value,
      pow(stability.value, -stabilityExponent),
      exp((1 - retrievability) * retrievabilityImpact) - 1,
      ratingFactor,
    ).product

    Stability(stability.value * (1 + compositeFactor))
  }

  private def getFuzzedInterval(interval: Duration, state: State): Duration = {
    val intervalDays = interval.toDays
    if (!enableFuzzing || state != State.Review || intervalDays < 2.5) {
      interval
    } else {
      val delta = Scheduler.FuzzRanges.foldLeft(1.0) { (acc, range) =>
        acc + range.factor * max(min(intervalDays, range.end) - range.start, 0.0)
      }

      val minDays = max(2, round(intervalDays - delta).toInt)
      val maxDays = min(round(intervalDays + delta).toInt, maximumInterval.toInt)
      val clampedMinDays = min(minDays, maxDays)

      val fuzzedDays = (random() * (maxDays - clampedMinDays + 1) + clampedMinDays).toInt
      val finalDays = min(fuzzedDays, maximumInterval.toInt)

      Duration.ofDays(finalDays)
    }
  }
}

private object Scheduler {
  private final case class FuzzRange(start: Double, end: Double, factor: Double)

  private val FuzzRanges: Vector[FuzzRange] = Vector(
    FuzzRange(2.5, 7.0, 0.15),
    FuzzRange(7.0, 20.0, 0.1),
    FuzzRange(20.0, Double.PositiveInfinity, 0.05)
  )
}