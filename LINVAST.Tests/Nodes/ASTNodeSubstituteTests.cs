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
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class ASTNodeSubstituteTests
    {
        [Test]
        public void BasicTest()
        {
            var ast1 = new IdListNode(1, new IdNode(1, "x"), new IdNode(1, "y"));
            var ast2 = new IdListNode(1, new IdNode(1, "X"), new IdNode(1, "y"));
            Assert.That(ast1.Substitute(new IdNode(2, "x"), new IdNode(2, "X")), Is.EqualTo(ast2));
        }

        [Test]
        public void MultipleSubstituteTest()
        {
            var ast1 = new IdListNode(1, new IdNode(1, "x"), new IdNode(1, "x"), new IdNode(1, "y"));
            var ast2 = new IdListNode(1, new IdNode(1, "X"), new IdNode(1, "X"), new IdNode(1, "y"));
            Assert.That(ast1.Substitute(new IdNode(2, "x"), new IdNode(2, "X")), Is.EqualTo(ast2));
        }

        [Test]
        public void ExpressionSubstituteTest()
        {
            var ast1 = new ArithmExprNode(1,
                new IdNode(1, "x"),
                ArithmOpNode.FromSymbol(1, "+"),
                new IdNode(1, "x")
            );
            var ast2 = new ArithmExprNode(1,
                new LitExprNode(1, 1),
                ArithmOpNode.FromSymbol(1, "+"),
                new LitExprNode(1, 1)
            );
            Assert.That(ast1.Substitute(new IdNode(2, "x"), new LitExprNode(2, 1)), Is.EqualTo(ast2));
        }

        [Test]
        public void SelfReferenceSubstituteTest()
        {
            var ast1 = new ArithmExprNode(1,
                new IdNode(1, "x"),
                ArithmOpNode.FromSymbol(1, "+"),
                new ArithmExprNode(1,
                    new IdNode(1, "y"),
                    ArithmOpNode.FromSymbol(1, "+"),
                    new IdNode(1, "x")
                )
            );

            var repl = new ArithmExprNode(1,
                new IdNode(1, "y"),
                new ArithmOpNode(1, "-", BinaryOperations.ArithmeticFromSymbol("-")),
                new IdNode(1, "x")
            );
            var ast2 = new ArithmExprNode(1,
                new IdNode(1, "x"),
                ArithmOpNode.FromSymbol(1, "+"),
                new ArithmExprNode(1,
                    repl,
                    ArithmOpNode.FromSymbol(1, "+"),
                    new IdNode(1, "x")
                )
            );

            Assert.That(ast1.Substitute(new IdNode(2, "y"), repl), Is.EqualTo(ast2));
        }

        [Test]
        public void SubstituteKeepsOriginalTest()
        {
            ASTNode ast1 = new IdListNode(1, new IdNode(1, "x"), new IdNode(1, "y"));
            ASTNode ast2 = new IdListNode(1, new IdNode(1, "X"), new IdNode(1, "y"));
            Assert.That(ast1.Substitute(new IdNode(2, "x"), new IdNode(2, "X")), Is.EqualTo(ast2));
            Assert.That(ast1, Is.EqualTo(new IdListNode(1, new IdNode(1, "x"), new IdNode(1, "y"))));
        }
    }
}
