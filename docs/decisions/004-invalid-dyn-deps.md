---
date: 2024-06-04
---
# How to invalidate tracked annotation dependencies?
## Context and Problem Statement
Based on [decision-002](002-dynamic-deps.md) and [decision-003](003-annotation-dyn-deps.md), some annotations being tracked in the dependency management framework may be invalidated, either from contingently computed results that were later discarded, or from legitimate validation errors of parent keywords.

An example of the second scenario is an annotation from within a `not` or `allOf` sub-schema. The annotation was valid at the time of collection within the keyword, but may be invalidated when evaluating the parent keyword.

How do we invalidate those annotations so dependants do not use them?

## Decision Drivers
* Simplest implementation that results in correct functionality
* Preserve the performance profile of the library so far

## Considered Options
* Require explicit invalidation

## Decision Outcome
Chosen option: "Require explicit invalidation".

### Consequences
* Failing to invalidate annotations may result in incorrect validations results if invalid annotations are used.

  The (mis)timing of the notifications could be the source of bugs if, for example, an annotation from keyword at `/a/b/c` is registered to satisfy a dependant at `/a/d`, and `/a/d` uses said dependency before `/a/b` is invalidated.
* The framework should help by auto-invalidating annotations from normal error results between the processing of vocabularies.
* Requires vocab implementations to explicitly notify the dependency management framework of contingently computed results that are invalid.

## More Information
### Regarding possible issues with the timing of invalidation
The current framework implementation processes vocabularies in whole, sequentially. It also takes care of invalidating annotations from legitimate error results between the processing of vocabularies.

This means that for inter-vocabulary dynamic dependencies, there are no issues as long as implementations correctly invalidate results. Based on the example above, if `/a/b` and `/a/d` are in different vocabularies, the potential bug would not present itself.

This still leaves the potential pitfall for intra-vocabulary dynamic dependencies that use the dependency management framework.

If `/a/b` and `/a/d` are in the same vocabulary instance, the implementation must take care to invalidate `/a/b` as necessary before `/a/d` retrieves the associated annotation. Similarly, it must check that `/a/b` is a valid result before retrieving descendant annotations since the framework auto-invalidation mechanism happens between the processing of vocabularies.

This last check could be skipped under two circumstances: when `/a/b` is known to not affect validation, like the `if` keyword; or if `/a/c` depended on an annotation at `/a/f` (direct sibling) because the annotation would not be registered if invalid.
