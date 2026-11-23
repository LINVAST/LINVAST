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

using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;

namespace LINVAST.Imperative.Builders.Common
{
    /// <summary>
    /// Shared implementation of <see cref="IExpressionSemantics"/>. Language subclasses only
    /// supply the per-language policy (truthiness, logical result, shift mode) and the
    /// operator-to-semantics mapping.
    /// </summary>
    public abstract class ExpressionSemanticsBase : IExpressionSemantics
    {
        protected abstract TruthinessMode Truthiness { get; }
        protected abstract LogicalResultMode? LogicalResult { get; }
        protected abstract bool JavaShifts { get; }
        public abstract ExpressionSemantics? ArithmeticSemantics(string symbol);

        public ArithmExprNode Arithmetic(int line, ExprNode left, ArithmOpNode op, ExprNode right)
            => new(line, left, op, right) { Semantics = this.ArithmeticSemantics(op.Symbol) };

        public LogicExprNode Logical(int line, ExprNode left, BinaryLogicOpNode op, ExprNode right)
            => new(line, left, op, right)
            {
                Semantics = new(truthiness: this.Truthiness, logicalResult: this.LogicalResult)
            };

        public UnaryExprNode Unary(int line, UnaryOpNode op, ExprNode operand)
            => new(line, op, operand)
            {
                Semantics = op.Symbol is "!" or "not" ? new(truthiness: this.Truthiness) : null
            };

        public CondExprNode Conditional(int line, ExprNode condition, ExprNode thenExpr, ExprNode elseExpr)
            => new(line, condition, thenExpr, elseExpr)
            {
                Semantics = new(truthiness: this.Truthiness)
            };

        public AssignExprNode Assignment(int line, ExprNode left, AssignOpNode op, ExprNode right)
            => new(line, left, op, right)
            {
                Semantics = op is ComplexAssignOpNode ? this.ArithmeticSemantics(op.Symbol[..^1]) : null
            };

        public AssignExprNode Assignment(int line, ExprNode left, ExprNode right)
            => new(line, left, right);
    }
}
