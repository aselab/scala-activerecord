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

  type NonPrimitiveJdbcMapper[P, A, T] = org.squeryl.dsl.NonPrimitiveJdbcMapper[P, A, T]
  type TDate = org.squeryl.dsl.TDate
  type TTimestamp = org.squeryl.dsl.TTimestamp
  type TOptionDate = org.squeryl.dsl.TOptionDate
  type TOptionTimestamp = org.squeryl.dsl.TOptionTimestamp
  type DeOptionizer[P1, A1, T1, A2 >: Option[A1] <: Option[A1], T2] =
    org.squeryl.dsl.DeOptionizer[P1, A1, T1, A2, T2]

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
