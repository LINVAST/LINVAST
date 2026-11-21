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


﻿using System.Linq;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class ASTNodeSubstituteTests
    {
        [Test]
        public void ReplacementOccurrencesAreIndependentAndDoNotReparentTheInput()
        {
            var original = new ExprListNode(1, new IdNode(1, "x"), new IdNode(1, "x"));
            var replacement = new IncExprNode(1, new IdNode(1, "x"));
            var replacementOwner = new ExprStatNode(1, replacement);

            var result = original.Substitute<ExprListNode>(new IdNode(1, "x"), replacement);
            Assert.That(result.Children[0], Is.EqualTo(replacement));
            Assert.That(result.Children[1], Is.EqualTo(replacement));
            Assert.That(result.Children[0], Is.Not.SameAs(replacement));
            Assert.That(result.Children[0], Is.Not.SameAs(result.Children[1]));
            Assert.That(replacement.Parent, Is.SameAs(replacementOwner));
            Assert.That(original.Children.Select(node => node.As<IdNode>().Identifier), Is.EqualTo(new[] { "x", "x" }));
            LoopInvariantTests.AssertOwnedTree(result);

            result.Children[0].As<IncExprNode>().Operator.As<AssignOpNode>().ApplyTo = (_, _) => 999;
            Assert.That(result.Children[1].As<IncExprNode>().Operator.As<AssignOpNode>().ApplyTo,
                Is.SameAs(replacement.Operator.As<AssignOpNode>().ApplyTo));
        }

        [Test]
        public void RootReplacementIsADetachedCopy()
        {
            var original = new IdNode(1, "x");
            var replacement = new ExprListNode(1, new IdNode(1, "y"));
            var owner = new ExprStatNode(1, replacement);
            var result = original.Substitute(new IdNode(2, "x"), replacement);
            Assert.That(result, Is.EqualTo(replacement));
            Assert.That(result, Is.Not.SameAs(replacement));
            Assert.That(result.Parent, Is.Null);
            Assert.That(replacement.Parent, Is.SameAs(owner));
            LoopInvariantTests.AssertOwnedTree(result);
        }

        [TestCase(true)]
        [TestCase(false)]
        public void NullSubstitutionStillCreatesAnIndependentTree(bool nullTarget)
        {
            var original = new ExprListNode(1, new IdNode(1, "x"));
            var owner = new ExprStatNode(1, original);
            var result = original.Substitute(nullTarget ? null : new IdNode(1, "x"), nullTarget ? new IdNode(1, "y") : null);
            Assert.That(result, Is.EqualTo(original));
            Assert.That(result, Is.Not.SameAs(original));
            Assert.That(result.Children[0], Is.Not.SameAs(original.Children[0]));
            Assert.That(result.Parent, Is.Null);
            Assert.That(original.Parent, Is.SameAs(owner));
            LoopInvariantTests.AssertOwnedTree(result);
        }

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

        [Test]
        public void DeleteStatementSubstituteTest()
        {
            ASTNode ast1 = new DeleteStatNode(1, new IdNode(1, "x"), new IdNode(1, "y"));
            ASTNode ast2 = new DeleteStatNode(1, new IdNode(1, "X"), new IdNode(1, "y"));

            Assert.That(ast1.Substitute(new IdNode(2, "x"), new IdNode(2, "X")), Is.EqualTo(ast2));
        }

        [Test]
        public void GlobalStatementSubstituteTest()
        {
            ASTNode ast1 = new GlobalStatNode(1, new IdNode(1, "x"), new IdNode(1, "y"));
            ASTNode ast2 = new GlobalStatNode(1, new IdNode(1, "X"), new IdNode(1, "y"));

            Assert.That(ast1.Substitute(new IdNode(2, "x"), new IdNode(2, "X")), Is.EqualTo(ast2));
        }

        [Test]
        public void NonlocalStatementSubstituteTest()
        {
            ASTNode ast1 = new NonlocalStatNode(1, new IdNode(1, "x"), new IdNode(1, "y"));
            ASTNode ast2 = new NonlocalStatNode(1, new IdNode(1, "X"), new IdNode(1, "y"));

            Assert.That(ast1.Substitute(new IdNode(2, "x"), new IdNode(2, "X")), Is.EqualTo(ast2));
        }
    }
}
