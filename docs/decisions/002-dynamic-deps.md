---
date: 2023-11-28
---
# How to implement validation for keywords with dynamic dependencies?
## Context and Problem Statement
Given that the chosen visitor framework is a push-model and given the chosen composite visitor approach, how to deal with keywords which depend on the result of other keywords?

Some examples are `else` which depends on the result of `if` in the same schema and vocabulary, and `unevalItems` which depends on several other keywords, possibly in the same schema or adjacent keywords sub-schemas but from a separate vocabulary.

## Decision Drivers
* Simplest implementation that results in correct functionality
* Retain the ability to validate structures in a streaming fashion (efficient mem over CPU)

## Considered Options
* Always compute keywords
* Buffer and replay

## Decision Outcome
Chosen option: "Always compute keywords", because it's the simplest solution that doesn't change the essence of the previous made decisions.

### Consequences
* Dynamic dependant keywords may be computed unnecessarily. For example, computing `if`, `then`, and `else` means one of those branches will always be computed and discarded.
* Leaves unsolved the annotation driven keywords like `unevalItems`.

## Pros and Cons of the Options
### Always compute keywords
* Simplest implementation that results in correctness.
* Mem over CPU: prefers efficient memory utilization by preserving the streaming style, while computation is wasted on discarded results.
* Biggest penalty if the discarded branches were CPU heavy (perhaps lots of `$ref`)

### Buffer and replay
* Not terribly complicated of an implementation.
* CPU over Mem: prefers efficient CPU utilization by only computing the branches that apply, while buffering the subject (sub)structure to be replayed and validated only if necessary.
* Biggest penalty if the (sub)structure is large.
* Could be re-used in the implementation of keywords that evaluate instance identity (`contains`, `const`, etc.) that need to "collect" the instance into some structure comparable with the Schema's expected value.
