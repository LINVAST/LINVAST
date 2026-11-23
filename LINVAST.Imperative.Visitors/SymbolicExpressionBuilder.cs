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
using System.Globalization;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using Newtonsoft.Json;
using Expr = MathNet.Symbolics.SymbolicExpression;

namespace LINVAST.Imperative.Visitors
{
    /// <summary>
    /// Builds a symbolic expression from an AST node using MathNet.Symbolics.
    /// </summary>
    public sealed class SymbolicExpressionBuilder : BaseASTVisitor<Expr>
    {
        private static readonly Regex _wildcardRegex = new(@"\bv__(?:[0-9a-f]{64}|[0-9]+)\b", RegexOptions.Compiled);

        /// <summary>
        /// Replaces complete wildcard variable names in an expression string with the specified replacement.
        /// Supports content-derived names and legacy numeric names.
        /// </summary>
        /// <param name="expr">The expression string containing wildcard variables.</param>
        /// <param name="replacement">The string to replace wildcards with. Defaults to "?".</param>
        /// <returns>The expression string with wildcards replaced.</returns>
        public static string WildcardReplace(string expr, string replacement = "?")
            => _wildcardRegex.Replace(expr, replacement);


        /// <summary>
        /// Gets or sets the AST node from which the symbolic expression is built.
        /// </summary>
        public ASTNode Node { get; set; }

        /// <summary>
        /// Initializes a new instance of the <see cref="SymbolicExpressionBuilder"/> class with the specified AST node.
        /// </summary>
        /// <param name="node">The AST node to build a symbolic expression from.</param>
        public SymbolicExpressionBuilder(ASTNode node)
        {
            this.Node = node;
        }


        /// <summary>
        /// Parses the AST node into a symbolic expression.
        /// </summary>
        /// <returns>The symbolic expression built from the AST node.</returns>
        public Expr Parse()
            => this.Visit(this.Node);

        public override Expr Visit(ASTNode node)
        {
            if (node is ExprNode expr) SemanticEvaluation.Validate(expr);
            return base.Visit(node);
        }

        public override Expr Visit(CondExprNode node)
        {
            if (TryConstant(node, out Expr result)) return result;
            return GetWildcard($"conditional:{JsonConvert.SerializeObject(node.Semantics)}:{this.Visit(node.Condition)}"
                + $":{this.Visit(node.ThenExpression)}:{this.Visit(node.ElseExpression)}");
        }


        /// <summary>
        /// Visits an arithmetic expression node, building a symbolic expression from its operands and operator.
        /// </summary>
        /// <param name="node">The arithmetic expression node to visit.</param>
        /// <returns>The symbolic expression representing the arithmetic operation.</returns>
        public override Expr Visit(ArithmExprNode node)
            => this.EvaluateBinaryExpression(node);

        /// <summary>
        /// Visits a relational expression node, building a symbolic expression from its operands and operator.
        /// </summary>
        /// <param name="node">The relational expression node to visit.</param>
        /// <returns>The symbolic expression representing the relational operation.</returns>
        public override Expr Visit(RelExprNode node)
            => this.EvaluateBinaryExpression(node);

        /// <summary>
        /// Visits a logic expression node, building a symbolic expression from its operands and operator.
        /// </summary>
        /// <param name="node">The logic expression node to visit.</param>
        /// <returns>The symbolic expression representing the logic operation.</returns>
        public override Expr Visit(LogicExprNode node)
            => this.EvaluateBinaryExpression(node);

        /// <summary>
        /// Visits a unary expression node, building a symbolic expression from its operand and operator.
        /// </summary>
        /// <param name="node">The unary expression node to visit.</param>
        /// <returns>The symbolic expression representing the unary operation.</returns>
        public override Expr Visit(UnaryExprNode node)
            => this.EvaluateUnaryExpression(node);

        /// <summary>
        /// Visits an identifier node, creating a symbolic variable from it.
        /// </summary>
        /// <param name="node">The identifier node to visit.</param>
        /// <returns>A symbolic variable expression.</returns>
        public override Expr Visit(IdNode node)
            => Expr.Variable(node.Identifier);

        /// <summary>
        /// Visits a function call expression node, creating a symbolic variable representing the function result.
        /// </summary>
        /// <param name="node">The function call expression node to visit.</param>
        /// <returns>A fresh opaque result. No function purity is assumed.</returns>
        public override Expr Visit(FuncCallExprNode node)
        {
            byte[] digest = SHA256.HashData(Encoding.UTF8.GetBytes(node.ToJson(compact: true)));
            return Expr.Variable($"r__{Convert.ToHexString(digest).ToLowerInvariant()}_{Guid.NewGuid():N}");
        }

        public override Expr Visit(CastExprNode node)
            => throw new EvaluationException("Cast evaluation requires source-language type semantics.");

        /// <summary>
        /// Visits a literal expression node, parsing its value into a symbolic expression.
        /// </summary>
        /// <param name="node">The literal expression node to visit.</param>
        /// <returns>A symbolic expression representing the literal value, or undefined if the value is null.</returns>
        public override Expr Visit(LitExprNode node)
            => Literal(node.Value);

        /// <summary>
        /// Visits a null literal expression node, returning an undefined symbolic expression.
        /// </summary>
        /// <param name="node">The null literal expression node to visit.</param>
        /// <returns>An undefined symbolic expression.</returns>
        public override Expr Visit(NullLitExprNode node)
            => Expr.Undefined;


        private Expr EvaluateBinaryExpression(BinaryExprNode node)
        {
            SemanticEvaluation.Validate(node);
            Expr left = this.Visit(node.LeftOperand);
            Expr right = this.Visit(node.RightOperand);
            if (node.Semantics is not null) {
                if (this.TryFoldInteger(node, out Expr result)) return result;
                return this.ParsePreserved($"({left}) {node.Operator.Symbol} ({right})");
            }
            return this.TryEvaluate(node, $"({left}) {node.Operator.Symbol} ({right})");
        }

        private Expr EvaluateUnaryExpression(UnaryExprNode node)
        {
            SemanticEvaluation.Validate(node);
            Expr operand = this.Visit(node.Operand);
            if (node.Semantics is not null) {
                if (this.TryFoldInteger(node, out Expr result)) return result;
                return this.ParsePreserved($"{node.Operator.Symbol}({operand})");
            }
            return this.TryEvaluate(node, $"{node.Operator.Symbol}({operand})");
        }

        private static Expr Literal(object? value)
            => value is null ? Expr.Undefined : value is string or char
                ? GetWildcard("literal:" + JsonConvert.SerializeObject(value))
                : Expr.Parse(Convert.ToString(value, CultureInfo.InvariantCulture));

        private bool TryFoldInteger(ExprNode node, out Expr result)
        {
            object? value = null;
            try { value = ConstantExpressionEvaluator.Evaluate(node); } catch { }
            if (value is sbyte or byte or short or ushort or int or uint or long or ulong) {
                try {
                    result = Expr.Parse(Convert.ToString(value, CultureInfo.InvariantCulture));
                    return true;
                } catch { }
            }
            result = Expr.Undefined;
            return false;
        }

        private static bool TryConstant(ExprNode node, out Expr result)
        {
            try {
                result = Literal(ConstantExpressionEvaluator.Evaluate(node));
                return true;
            } catch {
                result = Expr.Undefined;
                return false;
            }
        }

        private Expr ParsePreserved(string exprStr)
        {
            try { return Expr.Parse(exprStr); } catch { return GetWildcard(exprStr); }
        }

        private Expr TryEvaluate(ExprNode e, string exprStr)
        {
            Expr expr;
            object? constantValue = null;
            try {
                constantValue = ConstantExpressionEvaluator.Evaluate(e);
            } catch {

            }

            if (constantValue is not null) {
                try {
                    expr = Expr.Parse(System.Convert.ToString(constantValue, CultureInfo.InvariantCulture));
                    return expr;
                } catch {

                }
            }

            try {
                expr = Expr.Parse(exprStr);
            } catch {
                expr = GetWildcard(exprStr);
            }
            return expr;
        }

        private static Expr GetWildcard(string exprStr)
        {
            // Stable across builders without a process-wide cache, counter, or
            // retained expression history. The v__ namespace is reserved.
            byte[] digest = SHA256.HashData(Encoding.UTF8.GetBytes(exprStr));
            return Expr.Variable("v__" + Convert.ToHexString(digest).ToLowerInvariant());
        }
    }
}
