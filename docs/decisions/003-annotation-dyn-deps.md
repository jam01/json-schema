---
date: 2024-06-04
---
# How to collect annotations for dynamic dependencies?
## Context and Problem Statement
Some validation keywords are defined in terms of the annotation results of other keywords. How do we satisfy those dependencies in the context of output formats which do not preserve annotations, and user-provided annotation allow lists that may drop the internally required annotations?

## Decision Drivers
* Simplest implementation that results in correct functionality
* Preserve the performance profile of the library so far 

## Considered Options
* Collect all
* Collect allowed and internal
* Dependency management framework
* Dependency-aware collection

## Decision Outcome
Chosen option: "Dependency management framework", as it is the least complicated solution without requiring post-processing.

### Consequences
* Requires vocab implementations to use some framework methods in order to manage dependencies.
* Creates an issue where, based on [decision-002](002-dynamic-deps.md), some annotations being tracked may have been from contingently computed results that were later discarded.

### Confirmation
Working implementation of the 2020-12 _Unevaluated_ vocabulary and _Applicator_'s `contains` keyword, using this framework, passing the respective _test-suite_ tests.

## Pros and Cons of the Options
### Collect all
Ignore output formats that do not preserve annotations. Collect all annotations, ignoring the user-provided allow list, in order to satisfy dependant keywords. Finally, post-process the final result to strip the unwanted annotations and conform to the required output format.

* The simplest solution.
* Potentially creating and collecting unnecessary annotations until the post-process filtering step.
* Considering performance of hot-path output unit creation, this option would potentially create a lot of unnecessary objects that trigger garbage collection.

### Collect allowed and internal
Ignore output formats that do not preserve annotations. Collect only user-allowed and internally necessary annotations, in order to satisfy dependant keywords. Finally, post-process the final result to strip the internal annotations and conform to the required output format.

* Slightly more complicated than collecting all annotations, needing to track internal dependant keywords.
* More memory efficient as it should prevent _some_ object instantiation and collection.

### Dependency management framework
Create a specialized annotation dependency management framework. Each dependant keyword would declare their dependencies, throughout schema validation the framework would collect matching dependencies.

* Not as simple as just collecting all annotations, but not complicated conceptually or implementation-wise.
* Mem over CPU: Enables output formats that do not preserve annotations without post-processing results, avoiding object instantiation and collection. Some extra computations may happen matching dependencies.
* Computation penalty with combination of many dependants `d` and many annotations`a`; the framework must check `d` x `a` predicates.

### Dependency-aware collection
A specialized collection where the output units themselves are aware of the annotations it carries that are only for internal keyword dependants. As the dependant's scope is closed, units are notified and annotations are dropped.

* The most complex option; lots of "smarts" into otherwise plain output units, also requires units to be somewhat mutable.
* Mem over complexity: Enables output formats that do not preserve annotations without post-processing results, avoiding object instantiation and collection. Mentioned complexity.

## More Information
The memory tradeoff between all solutions is clear in favor of the chosen solution. However, the computational tradeoff is harder to quantify. Dependants in all other solutions would have to filter and/or traverse the result tree of its siblings to find the wanted annotations, which may end up being more computationally demanding.

For a discussion on annotation collection challenges in general, see: [Making annotation collection practical](https://github.com/orgs/json-schema-org/discussions/236)
