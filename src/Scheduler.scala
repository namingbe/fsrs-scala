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
  private val decay: Double = -parameters.decayRate
  private val factor: Double = pow(0.9, 1.0 / decay) - 1

  def getCardRetrievability(card: Card, at: Instant = Instant.now()): Double = {
    card.lastReview.fold(0.0) { last =>
      val elapsedDays = max(0, Duration.between(last, at).toDays)
      pow(1 + factor * elapsedDays / card.stability.get, decay)
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

  private def updateStateAndInterval(
    card: Card,
    rating: Rating,
    stability: Double
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

  private def initialStability(rating: Rating): Double = {
    val stability = parameters.initialStabilityFor(rating)
    clampStability(stability)
  }

  private def initialDifficulty(rating: Rating): Double = {
    val difficulty = parameters.baseDifficulty - exp(parameters.difficultyScale * (rating.value - 1)) + 1
    clampDifficulty(difficulty)
  }

  private def nextInterval(stability: Double): Long = {
    val interval = (stability / factor) * (pow(desiredRetention, 1.0 / decay) - 1)
    val days = round(interval)
    min(max(days, 1), maximumInterval)
  }

  private def shortTermStability(stability: Double, rating: Rating): Double = {
    val stabilityIncrease = exp(parameters.shortTermFactor * (rating.value - 3 + parameters.ratingOffset)) *
      pow(stability, -parameters.stabilityDiminish)

    val clampedIncrease = if (Set(Rating.Good, Rating.Easy).contains(rating)) {
      max(stabilityIncrease, 1.0)
    } else {
      stabilityIncrease
    }

    val newStability = stability * clampedIncrease
    clampStability(newStability)
  }

  private def nextDifficulty(difficulty: Double, rating: Rating): Double = {
    def linearDamping(deltaDifficulty: Double, difficulty: Double): Double = {
      (10.0 - difficulty) * deltaDifficulty / 9.0
    }

    def meanReversion(arg1: Double, arg2: Double): Double = {
      parameters.meanReversionWeight * arg1 + (1 - parameters.meanReversionWeight) * arg2
    }

    val arg1 = initialDifficulty(Rating.Easy)
    val deltaDifficulty = -(parameters.difficultyChangeRate * (rating.value - 3))
    val arg2 = difficulty + linearDamping(deltaDifficulty, difficulty)

    val newDifficulty = meanReversion(arg1, arg2)
    clampDifficulty(newDifficulty)
  }

  private def nextStability(difficulty: Double, stability: Double, retrievability: Double, rating: Rating): Double = {
    val newStability = if (rating == Rating.Again) {
      nextForgetStability(difficulty, stability, retrievability)
    } else {
      nextRecallStability(difficulty, stability, retrievability, rating)
    }
    clampStability(newStability)
  }

  private def nextForgetStability(difficulty: Double, stability: Double, retrievability: Double): Double = {
    val longTermParams = parameters.forgetFactor *
      pow(difficulty, -parameters.difficultyImpact) *
      (pow(stability + 1, parameters.stabilityGrowth) - 1) *
      exp((1 - retrievability) * parameters.forgetRetrievability)

    val shortTermParams = stability / exp(parameters.shortTermFactor * parameters.ratingOffset)

    min(longTermParams, shortTermParams)
  }

  private def nextRecallStability(difficulty: Double, stability: Double, retrievability: Double, rating: Rating): Double = {
    val hardPenalty = if (rating == Rating.Hard) parameters.hardPenalty else 1.0
    val easyBonus = if (rating == Rating.Easy) parameters.easyBonus else 1.0

    stability * (1 +
      exp(parameters.recallFactor) *
        (11 - difficulty) *
        pow(stability, -parameters.stabilityExponent) *
        (exp((1 - retrievability) * parameters.retrievabilityImpact) - 1) *
        hardPenalty *
        easyBonus)
  }

  private def getFuzzedInterval(interval: Duration, state: State): Duration = {
    if (!enableFuzzing || state != State.Review) {
      interval
    } else {
      val intervalDays = interval.toDays
      if (intervalDays < 2.5) {
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

  private def clampDifficulty(difficulty: Double): Double = {
    min(max(difficulty, Scheduler.MinDifficulty), Scheduler.MaxDifficulty)
  }

  private def clampStability(stability: Double): Double = {
    max(stability, SchedulerParameters.StabilityMin)
  }
}

private object Scheduler {
  private val MinDifficulty: Double = 1.0
  private val MaxDifficulty: Double = 10.0

  private final case class FuzzRange(start: Double, end: Double, factor: Double)

  private val FuzzRanges: Vector[FuzzRange] = Vector(
    FuzzRange(2.5, 7.0, 0.15),
    FuzzRange(7.0, 20.0, 0.1),
    FuzzRange(20.0, Double.PositiveInfinity, 0.05)
  )
}