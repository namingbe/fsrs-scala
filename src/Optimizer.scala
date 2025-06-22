package com.namingbe.fsrs

class Optimizer(reviewLogs: Vector[ReviewLog]) {
  def computeOptimalParameters(verbose: Boolean = false): Vector[Double] = ???

  def computeOptimalRetention(parameters: Vector[Double]): Double = ???

  private def computeBatchLoss(parameters: Vector[Double]): Double = ???

  private def computeProbsAndCosts(): Map[String, Double] = ???

  private def simulateCost(
    desiredRetention: Double,
    parameters: Vector[Double],
    numCardsSimulate: Int,
    probsAndCosts: Map[String, Double]
  ): Double = ???
}