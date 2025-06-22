# fsrs-scala

Scala implementation of FSRS.
I'm going to base this thing on the [official Python implementation](https://github.com/open-spaced-repetition/py-fsrs) and half-ass the living shit out of it.
Use at your own peril.

## Observations (I didn't bother reading the docs)

Card entity is more of a CardSchedulingState entity, an object that is everything except for the card contents themselves.

Retrievability ≈ predicted retention

Python lib spends a lot of lines on dealing with serialisation even though it does not handle storage.
For tests, I presume? Bruh.
Checked after writing this, it was for tests. Polluting implementation for tests.

My Claude is laughing his ass off from Torch being used to optimise 21 parameter.
