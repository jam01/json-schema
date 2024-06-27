---
date: 2024-06-24
---
# Which build tool to use?
## Context and Problem Statement
What build tool to use?

## Decision Drivers
* Quickest path to distribution
* Expertise with Maven
* Barrier of entry for would-be contributors
* Scala community mostly uses _sbt_ (some _mill_)
* Official support to create Scala.js distributions through _sbt_

## Considered Options
* Maven
* Gradle
* sbt
* Mill

## Decision Outcome
Chosen option: "Maven" because it _can_ create JVM and Scala.js artifacts. 

### Consequences
* Quickest path to distribution.
* Minor Maven setup complexity with adding `shared/` sources to both distributions.
* Two POMs, one for each distribution.
* No obvious way to test Scala.js running in a JavaScript environment (supported in _sbt_).

## More Information

Useful resources for Scala.js packaging:
* [@sjrd on StackOverflow: How to use scala.js from maven](https://stackoverflow.com/a/26524749/4814697)
* [Implementing Scala.js Support for Scala 3](https://www.scala-lang.org/2020/11/03/scalajs-for-scala-3.html)
* [Scala.js 1.16.0 sbt Plugin Implementation](https://github.com/scala-js/scala-js/blob/v1.16.0/sbt-plugin/src/main/scala/org/scalajs/sbtplugin/ScalaJSPluginInternal.scala#L788)
* [Setting Up Scala-js with maven](https://nitinnizhawan.com/javascript/scala/scala-js/maven/2016/01/01/setting-up-scala-js-with-maven)
