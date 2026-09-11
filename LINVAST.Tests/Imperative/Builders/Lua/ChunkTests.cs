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
using LINVAST.Imperative.Builders.Lua;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Lua
{
    internal sealed class ChunkTests : SourceComponentTestsBase
    {
        [Test]
        public void BasicTest()
        {
            SourceNode tu = this.AssertTranslationUnit(@"x = 2");
            Assert.That(tu.Children.Single(), Is.InstanceOf<DeclStatNode>());
        }

        [Test]
        public void FunctionTest()
        {
            SourceNode tu = this.AssertTranslationUnit(@"function two() return 2 end");
            Assert.That(tu.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void EmptyFunctionBodyTest()
        {
            SourceNode tu = this.AssertTranslationUnit(@"function noop() end");
            FuncNode func = tu.Children.Single().As<FuncNode>();
            Assert.That(func.Definition!.Children, Is.Empty);
        }

        [Test]
        public void FunctionWithReturnListTest()
        {
            SourceNode tu = this.AssertTranslationUnit(@"function pair() return 1, 2 end");
            FuncNode func = tu.Children.Single().As<FuncNode>();

            JumpStatNode ret = func.Definition!.Children.Single().As<JumpStatNode>();
            ExprListNode values = ret.ReturnExpr!.As<ExprListNode>();
            Assert.That(values.Expressions.Select(v => LINVAST.Imperative.Visitors.ConstantExpressionEvaluator.Evaluate(v)),
                Is.EqualTo(new object[] { 1, 2 }));
        }


        protected override ASTNode GenerateAST(string src)
            => new LuaASTBuilder().BuildFromSource(src);
    }
}
