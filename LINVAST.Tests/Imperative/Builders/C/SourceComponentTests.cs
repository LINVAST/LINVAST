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
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.C
{
    internal sealed class SourceComponentTests : SourceComponentTestsBase
    {
        [Test]
        public void BasicTest()
        {
            SourceNode sc = this.AssertTranslationUnit(@"void f() { }");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void MultipleFunctionsTest()
        {
            SourceNode sc = this.AssertTranslationUnit(@"
                int f(int x) { 
                    int y = 3;
                    return x + y;
                }

                static float st_x() {
                    return 3.5f;
                }
            ");
            Assert.That(sc.Children, Is.All.InstanceOf<FuncNode>());
        }

        [Test]
        public void MixedDeclarationTest()
        {
            SourceNode sc = this.AssertTranslationUnit(@"
                int f(int x) { 
                    int y = 3;
                    return x + y;
                }

                extern static unsigned int x, y = 5;

                static float st_x() {
                    return 3.5f;
                }
            ");
            Assert.That(sc.Children.ElementAt(0), Is.InstanceOf<FuncNode>());
            Assert.That(sc.Children.ElementAt(1), Is.InstanceOf<DeclStatNode>());
            Assert.That(sc.Children.ElementAt(2), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void EmptySourceTest()
        {
            this.AssertTranslationUnit("", empty: true);
        }


        protected override ASTNode GenerateAST(string src)
            => new CASTBuilder().BuildFromSource(src);
    }
}
