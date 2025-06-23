package com.namingbe.fsrs

final class Stability private (val value: Double) extends AnyVal

object Stability {
  def apply(value: Double): Stability =
    new Stability(math.max(Min, value))

  val Min: Double = 0.001
}