// LINVAST - Language-INVariant AST library
// Copyright (C) 2026 Ivan Ristović
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.


﻿using System;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;

namespace LINVAST.Imperative.Visitors
{
    /// <summary>
    /// Evaluates constant expressions in an AST to their compile-time values.
    /// </summary>
    public sealed class ConstantExpressionEvaluator : BaseASTVisitor<object?>
    {
        /// <summary>
        /// Evaluates the specified expression node to a constant value.
        /// </summary>
        /// <param name="node">The expression node to evaluate.</param>
        /// <returns>The evaluated constant value, or null if the expression is not constant.</returns>
        public static object? Evaluate(ExprNode node)
            => new ConstantExpressionEvaluator().Visit(node);

        /// <summary>
        /// Attempts to evaluate the specified expression node as a value of type <typeparamref name="T"/>.
        /// </summary>
        /// <typeparam name="T">The type to cast the evaluated value to.</typeparam>
        /// <param name="node">The expression node to evaluate.</param>
        /// <param name="result">When this method returns, contains the evaluated value cast to <typeparamref name="T"/> if successful.</param>
        /// <returns>True if the expression evaluated to a value of type <typeparamref name="T"/>; otherwise false.</returns>
        public static bool TryEvaluateAs<T>(ExprNode node, out T result)
        {
            object? res = new ConstantExpressionEvaluator().Visit(node);
            if (res is not null && res is T castRes) {
                result = castRes;
                return true;
            } else {
                result = default!;
                return false;
            }
        }


        /// <summary>
        /// Visits a conditional expression node, evaluating the condition and returning the appropriate branch.
        /// </summary>
        /// <param name="node">The conditional expression node to visit.</param>
        /// <returns>The value of the selected branch (then or else).</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when the condition is not a boolean value.</exception>
        public override object? Visit(CondExprNode node)
        {
            object? cond = this.Visit(node.Condition);
            object? @then = this.Visit(node.ThenExpression);
            object? @else = this.Visit(node.ElseExpression);

            if (cond is null || cond is not bool booleanCondition)
                throw new EvaluationException("Failed to evaluate ternary conditional operator condition");

            return booleanCondition ? @then : @else;
        }

        /// <summary>
        /// Visits an arithmetic expression node, applying the operator to the evaluated operands.
        /// </summary>
        /// <param name="node">The arithmetic expression node to visit.</param>
        /// <returns>The result of applying the operator to the operands.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when either operand is null.</exception>
        public override object? Visit(ArithmExprNode node)
        {
            (object? l, object? r) = this.VisitBinaryOperands(node);
            if (l is null || r is null || l is NullLitExprNode || r is NullLitExprNode)
                throw new EvaluationException("Null reference in expression");
            return node.Operator.As<ArithmOpNode>().ApplyTo(l, r);
        }

        /// <summary>
        /// Visits a relational expression node, applying the operator to the evaluated operands.
        /// </summary>
        /// <param name="node">The relational expression node to visit.</param>
        /// <returns>The result of applying the operator to the operands.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when either operand is null.</exception>
        public override object? Visit(RelExprNode node)
        {
            (object? l, object? r) = this.VisitBinaryOperands(node);
            if (l is null || r is null || l is NullLitExprNode || r is NullLitExprNode)
                throw new EvaluationException("Null reference in expression");
            if (l is bool || r is bool)
                return node.Operator.As<RelOpNode>().ApplyTo(Convert.ToBoolean(l), Convert.ToBoolean(r));
            return node.Operator.As<RelOpNode>().ApplyTo(l, r);
        }

        /// <summary>
        /// Visits a logic expression node, applying the operator to the evaluated operands.
        /// </summary>
        /// <param name="node">The logic expression node to visit.</param>
        /// <returns>The result of applying the operator to the operands.</returns>
        public override object? Visit(LogicExprNode node)
        {
            (object? l, object? r) = this.VisitBinaryOperands(node);
            return node.Operator.As<BinaryLogicOpNode>().ApplyTo(Convert.ToBoolean(l), Convert.ToBoolean(r));
        }

        /// <summary>
        /// Visits a unary expression node, applying the operator to the evaluated operand.
        /// </summary>
        /// <param name="node">The unary expression node to visit.</param>
        /// <returns>The result of applying the operator to the operand.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when the operand is null.</exception>
        public override object? Visit(UnaryExprNode node)
        {
            object op = this.Visit(node.Operand) ?? throw new EvaluationException("Null reference in expression");
            return node.Operator.ApplyTo(op);
        }

        /// <summary>
        /// Visits an increment expression node, adding 1 to the evaluated operand.
        /// </summary>
        /// <param name="node">The increment expression node to visit.</param>
        /// <returns>The result of adding 1 to the operand.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when the operand is null.</exception>
        public override object? Visit(IncExprNode node)
        {
            object op = this.Visit(node.Expr) ?? throw new EvaluationException("Null reference in expression");
            return BinaryOperations.AddPrimitive(op, 1);
        }

        /// <summary>
        /// Visits a decrement expression node, subtracting 1 from the evaluated operand.
        /// </summary>
        /// <param name="node">The decrement expression node to visit.</param>
        /// <returns>The result of subtracting 1 from the operand.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when the operand is null.</exception>
        public override object? Visit(DecExprNode node)
        {
            object op = this.Visit(node.Expr) ?? throw new EvaluationException("Null reference in expression");
            return BinaryOperations.SubtractPrimitive(op, 1);
        }

        /// <summary>
        /// Visits a literal expression node, returning its value.
        /// </summary>
        /// <param name="node">The literal expression node to visit.</param>
        /// <returns>The value of the literal, or null if the node is null.</returns>
        public override object? Visit(LitExprNode node)
            => node?.Value;

        /// <summary>
        /// Visits a null literal expression node, returning null.
        /// </summary>
        /// <param name="node">The null literal expression node to visit.</param>
        /// <returns>Always returns null.</returns>
        public override object? Visit(NullLitExprNode node)
            => null;

        /// <summary>
        /// Visits an identifier node, indicating that the expression is not constant.
        /// </summary>
        /// <param name="node">The identifier node to visit.</param>
        /// <returns>Always throws an exception.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Always thrown because identifiers are not constant expressions.</exception>
        public override object? Visit(IdNode node)
            => throw new EvaluationException("Expression is not constant.");


        private (object? left, object? right) VisitBinaryOperands(BinaryExprNode expr)
            => (this.Visit(expr.LeftOperand), this.Visit(expr.RightOperand));
    }
}
