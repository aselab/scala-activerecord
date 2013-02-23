package com.github.aselab.activerecord.inner

trait Types {
  type Schema = org.squeryl.Schema
  type Table[T] = org.squeryl.Table[T]
  type Query[T] = org.squeryl.Query[T]
  type Queryable[T] = org.squeryl.Queryable[T]
  type KeyedEntity[T] = org.squeryl.KeyedEntity[T]
  type KeyedEntityDef[T, K] = org.squeryl.KeyedEntityDef[T, K]
  type CompositeKey2[T1, T2] = org.squeryl.dsl.CompositeKey2[T1, T2]
  type SquerylOptimistic = org.squeryl.Optimistic
  type StaleUpdateException = org.squeryl.StaleUpdateException

  val LogicalBoolean = org.squeryl.dsl.ast.LogicalBoolean
  type LogicalBoolean = org.squeryl.dsl.ast.LogicalBoolean
  type EqualityExpression = org.squeryl.dsl.ast.EqualityExpression
  type ExpressionNode = org.squeryl.dsl.ast.ExpressionNode
  type UpdateAssignment = org.squeryl.dsl.ast.UpdateAssignment
}
