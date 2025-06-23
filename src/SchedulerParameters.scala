package com.namingbe.fsrs

final case class SchedulerParameters private (
  // Initial stability for each rating (0-3)
  initialAgainStability: Stability, // w[0] - Rating.Again
  initialHardStability: Stability,  // w[1] - Rating.Hard
  initialGoodStability: Stability,  // w[2] - Rating.Good
  initialEasyStability: Stability,  // w[3] - Rating.Easy

  // Difficulty parameters (4-7)
  baseDifficulty: Double,        // w[4] - Base difficulty for mean reversion
  difficultyScale: Double,       // w[5] - Initial difficulty exponential scale
  difficultyChangeRate: Double,  // w[6] - Rate of difficulty change
  meanReversionWeight: Double,   // w[7] - Weight for difficulty mean reversion

  // Recall stability parameters (8-10)
  recallFactor: Double,          // w[8] - Base exponential factor for recall
  stabilityExponent: Double,     // w[9] - Stability diminishing exponent
  retrievabilityImpact: Double,  // w[10] - Retrievability impact on stability

  // Forget stability parameters (11-14)
  forgetFactor: Double,          // w[11] - Base factor for forget stability
  difficultyImpact: Double,      // w[12] - Difficulty impact (negative exp)
  stabilityGrowth: Double,       // w[13] - Stability growth factor
  forgetRetrievability: Double,  // w[14] - Retrievability impact on forget

  // Grade adjustments (15-16)
  hardPenalty: Double,           // w[15] - Penalty for Hard rating
  easyBonus: Double,             // w[16] - Bonus for Easy rating

  // Short-term stability (17-19)
  shortTermFactor: Double,       // w[17] - Base factor for same-day reviews
  ratingOffset: Double,          // w[18] - Rating adjustment offset
  stabilityDiminish: Double,     // w[19] - Stability diminishing factor

  // Decay parameter (20)
  decayRate: Double              // w[20] - Forgetting curve decay rate
)

object SchedulerParameters {
  def apply(v: Vector[Double]): Option[SchedulerParameters] = ???  // validate against bounds

  val DefaultParameters: SchedulerParameters = SchedulerParameters(
    initialAgainStability = Stability(0.2172),
    initialHardStability = Stability(1.1771),
    initialGoodStability = Stability(3.2602),
    initialEasyStability = Stability(16.1507),
    baseDifficulty = 7.0114, difficultyScale = 0.57, difficultyChangeRate = 2.0966, meanReversionWeight = 0.0069,
    recallFactor = 1.5261, stabilityExponent = 0.112, retrievabilityImpact = 1.0178,
    forgetFactor = 1.849, difficultyImpact = 0.1133, stabilityGrowth = 0.3127, forgetRetrievability = 2.2934,
    hardPenalty = 0.2191, easyBonus = 3.0004,
    shortTermFactor = 0.7536, ratingOffset = 0.3332, stabilityDiminish = 0.1437,
    decayRate = 0.2
  )

  val LowerBoundsParameters: Vector[Double] = Vector(
    Stability.Min, Stability.Min, Stability.Min, Stability.Min, 1.0, 0.001, 0.001, 0.001, 0.0, 0.0,
    0.001, 0.001, 0.001, 0.001, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.1
  )

  val InitialStabilityMax: Double = 100.0

  val UpperBoundsParameters: Vector[Double] = Vector(
    InitialStabilityMax, InitialStabilityMax, InitialStabilityMax, InitialStabilityMax, 10.0, 4.0, 4.0, 0.75, 4.5, 0.8,
    3.5, 5.0, 0.25, 0.9, 4.0, 1.0, 6.0, 2.0, 2.0, 0.8, 0.8
  )
}