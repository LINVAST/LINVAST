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
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Common
{
    internal abstract class ControlFlowTestsBase : ASTBuilderTestBase
    {
        protected IfStatNode AssertIfStatement(string src, object condValue, int thenStatementCount = 1, int? elseStatementCount = null)
        {
            IfStatNode node = this.GenerateAST(src).As<IfStatNode>();
            Assert.That(node, Is.Not.Null);
            this.AssertChildrenParentProperties(node);
            Assert.That(ConstantExpressionEvaluator.Evaluate(node.Condition), Is.EqualTo(condValue));
            Assert.That(node.ThenStat.Children, Has.Exactly(thenStatementCount).Items);
            if (elseStatementCount is not null) {
                Assert.That(node.ElseStat, Is.Not.Null);
                Assert.That(node.ElseStat!.Children, Has.Exactly(elseStatementCount.Value).Items);
            } else {
                Assert.That(node.ElseStat, Is.Null);
            }
            return node;
        }

        protected WhileStatNode AssertWhileStatement(string src, object condValue, int statCount = 1)
        {
            WhileStatNode node = this.GenerateAST(src).As<WhileStatNode>();
            Assert.That(node, Is.Not.Null);
            this.AssertChildrenParentProperties(node);
            Assert.That(ConstantExpressionEvaluator.Evaluate(node.Condition), Is.EqualTo(condValue));
            if (node.Statement is BlockStatNode block)
                Assert.That(block.Children, Has.Exactly(statCount).Items);
            else
                Assert.That(node.Statement.Children, Has.Exactly(statCount).Items);
            return node;
        }
    }
}
