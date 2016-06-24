package com.github.aselab.activerecord.inner

trait Types {
  type Schema = org.squeryl.CustomSchema
  type Table[T] = org.squeryl.Table[T]
  type Query[T] = org.squeryl.Query[T]
  type Queryable[T] = org.squeryl.Queryable[T]
  type QueryYield[T] = org.squeryl.dsl.QueryYield[T]
  type KeyedEntity[T] = org.squeryl.KeyedEntity[T]
  type KeyedEntityDef[T, K] = org.squeryl.KeyedEntityDef[T, K]
  type CompositeKey2[T1, T2] = org.squeryl.dsl.CompositeKey2[T1, T2]
  type CanLookup = org.squeryl.CanLookup
  type SquerylOptimistic = org.squeryl.Optimistic
  type StaleUpdateException = org.squeryl.StaleUpdateException
  type WhereState[A] = org.squeryl.dsl.fsm.WhereState[A]
  type Unconditioned = org.squeryl.dsl.fsm.Unconditioned

  val LogicalBoolean = org.squeryl.dsl.ast.LogicalBoolean
  type LogicalBoolean = org.squeryl.dsl.ast.LogicalBoolean
  type EqualityExpression = org.squeryl.dsl.ast.EqualityExpression
  type OrderByExpression = org.squeryl.dsl.ast.OrderByExpression
  type TypedExpressionFactory[A1, A2] = org.squeryl.dsl.TypedExpressionFactory[A1, A2]
  type TypedExpression[A1, A2] = org.squeryl.dsl.TypedExpression[A1, A2]
  type TOption = org.squeryl.dsl.TOption
  type TOptionFloat = org.squeryl.dsl.TOptionFloat
  type TNumericLowerTypeBound = org.squeryl.dsl.TNumericLowerTypeBound
  type OrderByArg = org.squeryl.dsl.ast.OrderByArg
  type ExpressionNode = org.squeryl.dsl.ast.ExpressionNode
  type UpdateAssignment = org.squeryl.dsl.ast.UpdateAssignment
  type GroupByState[R] = org.squeryl.dsl.fsm.GroupByState[R]
}
