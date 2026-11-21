package com.github.aselab.activerecord

import org.mockito.Mockito
import scala.reflect.ClassTag
import scala.language.implicitConversions

/**
 * Minimal replacement for specs2-mock (which has no Scala 3 build), using Mockito directly.
 * Supports the `mock[T]`, `call returns value` and `verifyOnce(mock).call(...)` styles used in these specs.
 */
trait MockSupport {
  def mock[T](implicit ct: ClassTag[T]): T =
    Mockito.mock(ct.runtimeClass.asInstanceOf[Class[T]])

  /** Asserts that the method called on the returned object was invoked exactly once on `m`. */
  def verifyOnce[T](m: T): T = Mockito.verify(m, Mockito.times(1))

  implicit def toStubbing[T](call: T): MockSupport.Stubbing[T] = new MockSupport.Stubbing(call)
}

object MockSupport {
  class Stubbing[T](call: T) {
    def returns(value: T): Unit = {
      Mockito.when(call).thenReturn(value)
      ()
    }
  }
}
