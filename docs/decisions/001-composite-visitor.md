---
date: 2023-10-21
---
# How to model vocabulary and sub-schema validation?
## Context and Problem Statement
Given that the chosen visitor framework is a push-model, how do we push the instance nodes to multiple vocab validators and also multiple keyword sub-schema validators (e.g: `allOf`).

## Considered Options
* Composite Visitor

## Decision Outcome
Chosen option: "Composite Visitor", basically tracking multiple visitors and sequentially forwarding nodes to all.

### Consequences
* Reusable solution for processing multiple vocabularies, multiple keyword sub-schemas, and also composable as with keywords like `allOf` that can use the same.
* All visitors operate on the same node; efficient memory usage.
* Leaves unsolved the validation of keywords with dynamic dependencies within same, and across, vocabularies.

## More Information
Use of Composite Visitors in applicator keywords like `allOf` should be implemented in a specialized way that short-circuits validation when a branch is found to be invalid and short-circuit assertion is enabled.
