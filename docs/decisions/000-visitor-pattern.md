---
date: 2023-10-12
---
# What form or pattern to use for implementation?
## Context and Problem Statement
How to implement the validator? Should we expect the instance to be a fully formed JSON ADT, like `Jackson.Node` or `ujson.Value` or some other structure? How do we model a Schema? How do we implement, group and select keyword validation behavior to be applied?

## Decision Drivers
* Support a performant processing style where we could start constructing an object as it's being validated
* Familiarity with `upickle.core.Visitor`
* Seeming direction of JSON Schema to drive validation through vocabularies
* Ideally something that can be used in JavaScript for frontend validation
* Developing this library in order to support another project

## Considered Options
* Model instance ADT
* Develop specific visitor framework
* Use `upickle` visitor framework
* Reify keywords individually
* Model keywords in vocabularies
* Model Schema as an ADT

## Decision Outcome
Chosen options: "Use `upickle` visitor framework", "Model keywords in vocabularies", and "Model Schema as an ADT".

Choosing `upickle` allows us to quickly develop the library by skipping
the creation and maintenance of a custom instance ADT and visitor framework. It also opens the door to leveraging Scala.js for frontend support.

Modeling keywords in vocabularies should be in-line with the direction of the JSON Schema specification, with the benefit of less complexity by not dealing with selecting keyword objects individually, but whole vocabs.

### Consequences
* Effectively means using Scala for the implementation, which is great because it enables both JVM and JavaScript uses.
* The `upickle` visitor framework enables validation of any JSON-like structure.
* Scala does present a higher learning curve for any would-be contributors, also _sbt_ is a mystery to me at this point.
* Enables the validation of potentially very large structures in a streaming fashion; without loading it entirely into memory.
* Complicates processing keywords that have dynamic dependencies and keywords that evaluate instance identity.

### Confirmation
Working implementation of the 2020-12 specification passing the entire mandated _test-suite_ plus optional _format_ tests.

## More Information
This decision represents the essence of the implementation, reconsidering this decisions ought to result in a different project.
