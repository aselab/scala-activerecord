package com.github.aselab.activerecord

import org.specs2.execute.{AsResult, Result, Success}

/**
 * specs2 5 removed `org.specs2.specification.Scope`. This keeps the `"example" >> new TestData { ... }` style:
 * the body runs while the scope is constructed, and failed expectations throw, so the example
 * succeeds if construction completes.
 */
trait Scope

object Scope {
  given [S <: Scope]: AsResult[S] with {
    def asResult(s: => S): Result = { s; Success() }
  }
}
