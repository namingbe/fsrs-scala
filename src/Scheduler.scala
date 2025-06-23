package com.namingbe.fsrs

import Card.State
import ReviewLog.Rating
import java.time.{Duration, Instant}
import scala.math.*

class Scheduler(
  parameters: SchedulerParameters = SchedulerParameters.DefaultParameters,
  desiredRetention: Double = 0.9,
  learningSteps: List[Duration] = List(Duration.ofMinutes(1), Duration.ofMinutes(10)),
  relearningSteps: List[Duration] = List(Duration.ofMinutes(10)),
  maximumInterval: Long = 36500,
  enableFuzzing: Boolean = true,
) {
  import parameters._

  private val decay: Double = -decayRate
  private val factor: Double = pow(0.9, 1.0 / decay) - 1

  def retrievabilityOf(card: Card, at: Instant = Instant.now()): Double = {
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
    val (newStability, newDifficulty) = updateStabilityAndDifficulty(card, rating, reviewedAt)
    val (nextState, nextInterval) = updateStateAndInterval(card, rating, newStability)
    val dueAt = reviewedAt.plus(getFuzzedInterval(nextInterval, nextState))

    (Card(card.cardId, nextState, Some(newStability), Some(newDifficulty), dueAt, Some(reviewedAt)),
      ReviewLog(card.cardId, rating, reviewedAt))
  }

  private def updateStabilityAndDifficulty(
    card: Card,
    rating: Rating,
    reviewedAt: Instant,
  ): (Stability, Difficulty) = {
    if (card.stability.isEmpty || card.difficulty.isEmpty) { // First review
      (initialStability(rating), initialDifficulty(rating))
    } else {
      val newStability = if (card.lastReview.exists(Duration.between(_, reviewedAt).toDays < 1)) { // Same day
        shortTermStability(card.stability.get, rating)
      } else {
        nextStability(card.difficulty.get, card.stability.get, retrievabilityOf(card, reviewedAt), rating)
      }
      val newDifficulty = nextDifficulty(card.difficulty.get, rating)
      (newStability, newDifficulty)
    }
  }

  private def updateStateAndInterval(
    card: Card,
    rating: Rating,
    stability: Stability
  ): (State, Duration) = {
    val stepSequence = if card.lastReview.isEmpty then learningSteps else relearningSteps

    def stepBasedTransition(currentStep: Int): (State, Duration) = {
      if (currentStep >= stepSequence.length && rating != Rating.Again) {
        // Graduating from step-based
        (State.Review, Duration.ofDays(nextInterval(stability)))
      } else {
        // Stay in StepBased state with step-based intervals
        rating match {
          case Rating.Again => (State.StepBased(0), stepSequence.head)
          case Rating.Hard if currentStep == 0 =>
            val interval = stepSequence match {
              case Nil => ??? // how would the original handle empty step sequences?
              case firstStep :: Nil => Duration.ofMillis((firstStep.toMillis * 1.5).toLong)
              case firstStep :: secondStep :: _ => Duration.ofMillis((firstStep.toMillis + secondStep.toMillis) / 2)
            }
            (State.StepBased(currentStep), interval)
          case Rating.Hard => (State.StepBased(currentStep), stepSequence(currentStep))
          case Rating.Good if currentStep + 1 == stepSequence.length => (State.Review, Duration.ofDays(nextInterval(stability)))
          case Rating.Good => (State.StepBased(currentStep + 1), stepSequence(currentStep + 1))
          case Rating.Easy => (State.Review, Duration.ofDays(nextInterval(stability)))
        }
      }
    }

    def reviewTransition: (State, Duration) = {
      rating match {
        case Rating.Again if relearningSteps.nonEmpty => (State.StepBased(0), relearningSteps.head)
        case _ => (State.Review, Duration.ofDays(nextInterval(stability)))
      }
    }

    card.state match {
      case State.StepBased(currentStep) => stepBasedTransition(currentStep)
      case State.Review => reviewTransition
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
    val stabilityIncrease =
      exp(shortTermFactor * (rating.value - 3 + ratingOffset)) * pow(stability.value, -stabilityDiminish)
    val clampedIncrease = rating match {
      case Rating.Good | Rating.Easy => max(stabilityIncrease, 1.0)
      case _ => stabilityIncrease
    }
    Stability(stability.value * clampedIncrease)
  }

  private def nextDifficulty(difficulty: Difficulty, rating: Rating): Difficulty = {
    def linearDamping(deltaDifficulty: Double): Double = {
      (10.0 - difficulty.value) * deltaDifficulty / 9.0
    }

    val arg1 = initialDifficulty(Rating.Easy)
    val deltaDifficulty = -(difficultyChangeRate * (rating.value - 3))
    val arg2 = difficulty.value + linearDamping(deltaDifficulty)
    Difficulty(meanReversionWeight * arg1.value + (1 - meanReversionWeight) * arg2)
  }

  private def nextStability(difficulty: Difficulty, stability: Stability, retrievability: Double, rating: Rating): Stability = {
    def nextForgetStability: Stability = {
      val shortTerm = stability.value / exp(shortTermFactor * ratingOffset)
      val longTerm = List(
        forgetFactor,
        pow(difficulty.value, -difficultyImpact),
        pow(stability.value + 1, stabilityGrowth) - 1,
        exp((1 - retrievability) * forgetRetrievability),
      ).product
      Stability(min(shortTerm, longTerm))
    }

    def nextRecallStability: Stability = {
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

    rating match {
      case Rating.Again => nextForgetStability
      case _ => nextRecallStability
    }
  }

  private def getFuzzedInterval(interval: Duration, state: State): Duration = {
    val intervalDays = interval.toDays
    if (enableFuzzing && state == State.Review && intervalDays >= 2.5) {
      val delta = Scheduler.FuzzRanges.foldLeft(1.0) { (acc, range) =>
        acc + range.factor * max(min(intervalDays.toDouble, range.end) - range.start, 0.0)
      }

      val minDays = max(2, round(intervalDays - delta).toInt)
      val maxDays = min(round(intervalDays + delta).toInt, maximumInterval.toInt)
      val clampedMinDays = min(minDays, maxDays)

      val fuzzedDays = (random() * (maxDays - clampedMinDays + 1) + clampedMinDays).toInt
      val finalDays = min(fuzzedDays, maximumInterval.toInt)

      Duration.ofDays(finalDays)
    } else {
      interval
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