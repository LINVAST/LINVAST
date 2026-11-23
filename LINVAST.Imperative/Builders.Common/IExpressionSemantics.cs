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
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Common
{
    /// <summary>
    /// Language-specific policy for building expression nodes and their
    /// source-language semantics metadata.
    /// </summary>
    public interface IExpressionSemantics
    {
        /// <summary>
        /// Returns the semantics metadata for an arithmetic operator symbol, or null when the
        /// symbol carries no language-specific semantics.
        /// </summary>
        ExpressionSemantics? ArithmeticSemantics(string symbol);

        ArithmExprNode Arithmetic(int line, ExprNode left, ArithmOpNode op, ExprNode right);
        LogicExprNode Logical(int line, ExprNode left, BinaryLogicOpNode op, ExprNode right);
        UnaryExprNode Unary(int line, UnaryOpNode op, ExprNode operand);
        CondExprNode Conditional(int line, ExprNode condition, ExprNode thenExpr, ExprNode elseExpr);
        AssignExprNode Assignment(int line, ExprNode left, AssignOpNode op, ExprNode right);
        AssignExprNode Assignment(int line, ExprNode left, ExprNode right);
    }
}
