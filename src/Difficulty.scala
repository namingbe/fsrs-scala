package com.namingbe.fsrs

final class Difficulty private(val value: Double) extends AnyVal

object Difficulty {
  def apply(value: Double): Difficulty =
    new Difficulty(math.min(math.max(Min, value), Max))

  private val Min: Double = 1.0
  private val Max: Double = 10.0
}