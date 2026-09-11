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


﻿using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using LINVAST.Nodes;
using NUnit.Framework;
using Expr = MathNet.Symbolics.SymbolicExpression;

namespace LINVAST.Tests.Visitors
{
    internal sealed class SymbolicExpressionBuilderTests
    {
        [Test]
        public void ConstantTests()
        {
            this.AssertParse(new LitExprNode(1, 3), Expr.Parse("3"));
            this.AssertParse(new LitExprNode(1, 4.3), Expr.Parse("4.3"));
            this.AssertParse(new NullLitExprNode(1), Expr.Undefined);
        }

        [Test]
        public void VariableTests()
        {
            this.AssertParse(new IdNode(1, "x"), Expr.Variable("x"));
            this.AssertParse(new IdNode(1, "xyz"), Expr.Variable("xyz"));
        }

        [Test]
        public void ArithmeticExpressionTests()
        {
            this.AssertParse(
                new ArithmExprNode(1,
                    new LitExprNode(1, 3),
                    ArithmOpNode.FromSymbol(1, "+"),
                    new LitExprNode(1, 1)
                ),
                Expr.Parse("4")
            );

            this.AssertParse(
                new ArithmExprNode(1,
                    new IdNode(1, "x"),
                    ArithmOpNode.FromSymbol(1, "+"),
                    new LitExprNode(1, 1)
                ),
                Expr.Parse("1 + x")
            );

            this.AssertParse(
                new ArithmExprNode(1,
                    new IdNode(1, "x"),
                    ArithmOpNode.FromSymbol(1, "+"),
                    new ArithmExprNode(1,
                        new IdNode(1, "x"),
                        ArithmOpNode.FromSymbol(1, "-"),
                        new LitExprNode(1, 1)
                    )
                ),
                Expr.Parse("-1 + 2*x")
            );

            this.AssertWildcardParse(
                new ArithmExprNode(1,
                    new IdNode(1, "x"),
                    ArithmOpNode.FromSymbol(1, "+"),
                    new ArithmExprNode(1,
                        new IdNode(1, "x"),
                        ArithmOpNode.FromBitwiseSymbol(1, "<<"),
                        new LitExprNode(1, 1)
                    )
                ),
                "? + x"
            );
        }

        [Test]
        public void LogicExpressionTests()
        {
            this.AssertParse(
                new LogicExprNode(1,
                    new LitExprNode(1, true),
                    BinaryLogicOpNode.FromSymbol(1, "&&"),
                    new LitExprNode(1, false)
                ),
                "False"
            );

            this.AssertParse(
                new LogicExprNode(1,
                    new LitExprNode(1, true),
                    BinaryLogicOpNode.FromSymbol(1, "&&"),
                    new LogicExprNode(1,
                        new LitExprNode(1, false),
                        BinaryLogicOpNode.FromSymbol(1, "||"),
                        new LitExprNode(1, true)
                    )
                ),
                "True"
            );

            this.AssertWildcardParse(
                new LogicExprNode(1,
                    new IdNode(1, "x"),
                    BinaryLogicOpNode.FromSymbol(1, "&&"),
                    new LitExprNode(1, 1)
                ),
                "?"
            );
        }


        private void AssertParse(ASTNode node, Expr expected)
            => this.AssertParse(node, expected.ToString());

        private void AssertParse(ASTNode node, string expected)
            => Assert.That(this.Parse(node).ToString(), Is.EqualTo(expected));

        private void AssertWildcardParse(ASTNode node, string expected)
        {
            string wildcardExpr = SymbolicExpressionBuilder.WildcardReplace(this.Parse(node).ToString());
            Assert.That(wildcardExpr, Is.EqualTo(expected.ToString()));
        }

        private Expr Parse(ASTNode node)
            => new SymbolicExpressionBuilder(node).Parse();
    }
}
