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


﻿using System.Collections.Generic;
using System.Linq;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using MathNet.Symbolics;
using Expr = MathNet.Symbolics.SymbolicExpression;

namespace LINVAST.Imperative.Visitors
{
    /// <summary>
    /// Evaluates symbolic expressions by substituting variables with provided symbolic values.
    /// </summary>
    public sealed class ExpressionEvaluator
    {
        /// <summary>
        /// Evaluates an expression node by building a symbolic expression and substituting variables.
        /// </summary>
        /// <param name="node">The expression node to evaluate.</param>
        /// <param name="symbols">A dictionary mapping variable names to their symbolic expressions.</param>
        /// <returns>The reduced symbolic expression.</returns>
        public static Expr TryEvaluate(ExprNode node, Dictionary<string, Expr> symbols)
            => TryEvaluate(new SymbolicExpressionBuilder(node).Parse(), symbols);

        /// <summary>
        /// Evaluates a symbolic expression by substituting all variables with their provided values.
        /// </summary>
        /// <param name="expr">The symbolic expression to evaluate.</param>
        /// <param name="symbols">A dictionary mapping variable names to their symbolic expressions.</param>
        /// <returns>The reduced symbolic expression with all known variables substituted.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when an infinite cycle is detected or an undefined variable remains.</exception>
        public static Expr TryEvaluate(Expr expr, Dictionary<string, Expr> symbols)
        {
            IEnumerable<Expr> vars = expr.CollectVariables();
            bool canReduce = true;
            for (int i = 0; canReduce && vars.Any(); i++) {
                if (i > symbols.Count)
                    throw new EvaluationException("Infinite cycle detected.");
                canReduce = false;
                foreach (Expr v in vars) {
                    string varStr = v.VariableName;
                    if (symbols.ContainsKey(varStr)) {
                        expr = expr.Substitute(v, symbols[varStr]);
                        canReduce = true;
                    }
                }
                vars = expr.CollectVariables();
            }

            if (expr.Type == SymbolicExpressionType.Undefined)
                throw new EvaluationException("Undefined variable found in expression");

            return expr;
        }
    }
}
