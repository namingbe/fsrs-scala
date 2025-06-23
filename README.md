# fsrs-scala

Scala implementation of FSRS.
I'm going to base this thing on the [official Python implementation](https://github.com/open-spaced-repetition/py-fsrs) and half-ass the living shit out of it.
Use at your own peril.

Since this is for my personal use rn, this is how to install it to the local repo
```shell
rm -rf ~/.ivy2/local/com.namingbe/fsrs-scala_3*
sbt publishLocal
```

And this is how to hook it up from another project
```sbt
libraryDependencies += "com.namingbe" %% "fsrs-scala" % "0.1.0"
```

The abstraction and interfaces are shit so if you're actually using that. Get help.

## Observations (I didn't bother reading the docs)

Card entity is more of a CardSchedulingState entity, an object that is everything except for the card contents themselves.

Retrievability ≈ predicted retention

Python lib spends a lot of lines on dealing with serialisation even though it does not handle storage.
For tests, I presume? Bruh.
Checked after writing this, it was for tests. Polluting implementation for tests.

My Claude is laughing his ass off from Torch being used to optimise 21 parameter.
Speaking of, **everything below is Claude's yapping**.

## Implementation Notes

This implementation follows the FSRS v6 mathematical formulas directly, with some key abstraction differences from the Python reference:

### Core Algorithm Structure
The algorithm reduces to three clean phases:
1. **Memory Update**: Calculate new stability and difficulty based on review performance
2. **State/Interval Calculation**: Determine next card state and base interval
3. **Fuzzing**: Apply randomization to Review intervals (when enabled)

### Key Abstractions

**State Management**: Uses a two-state ADT (`StepBased(step)` | `Review`) instead of the Python's three-state enum. Learning and Relearning are algorithmically identical - they both use step-based intervals and the same memory update formulas.

**Parameter Handling**: Structured case class with meaningful names (`baseDifficulty`, `hardPenalty`, etc.) rather than raw tuple indexing (`w[4]`, `w[15]`).

**Functional Design**: Pure functions for all calculations. The Python version uses imperative card mutation throughout a 200+ line method; we compute values functionally and construct the final card once.

**Implementation Fidelity**: Each mathematical formula maps to a single, focused function. No serialization, no test utilities, no optimization framework dependencies - just the scheduling algorithm.
