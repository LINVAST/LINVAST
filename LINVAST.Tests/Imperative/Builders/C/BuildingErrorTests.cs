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
using System.IO;
using LINVAST.Exceptions;
using LINVAST.Imperative;
using LINVAST.Imperative.Builders.C;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.C
{
    internal sealed class BuildingErrorTests : BuildingErrorTestsBase
    {
        [Test]
        public void SourceNotFoundTest()
        {
            Assert.Throws<FileNotFoundException>(() => new ImperativeASTFactory().BuildFromFile("404.c"));
        }

        [Test]
        public void InvalidDeclarationTests()
        {
            this.AssertThrows<SyntaxErrorException>("void f { };");
            this.AssertThrows<SyntaxErrorException>("void ();");
            this.AssertThrows<SyntaxErrorException>("int f(int, int x);");
            this.AssertThrows<SyntaxErrorException>("int f(0 x);");
            this.AssertThrows<SyntaxErrorException>("int f(3);");
            this.AssertThrows<SyntaxErrorException>("int f(int[] x);");
            this.AssertThrows<SyntaxErrorException>("int[] f(int x);");
            this.AssertThrows<SyntaxErrorException>("int f[](int x,,);");
            this.AssertThrows<SyntaxErrorException>("int f(int x, int y,);");
            this.AssertThrows<SyntaxErrorException>("int f(int x, int y){}{};");
            this.AssertThrows<SyntaxErrorException>("int x = ;;");
            this.AssertThrows<SyntaxErrorException>("int x = .3, 2..;");
            this.AssertThrows<SyntaxErrorException>("int x = ..3;;");
            this.AssertThrows<SyntaxErrorException>("int x = ();");
        }

        [Test]
        public void InvalidExpressionTests()
        {
            this.AssertThrows<SyntaxErrorException>("int x = 1 +;");
            this.AssertThrows<SyntaxErrorException>("int x = 1 // 2;");
            this.AssertThrows<SyntaxErrorException>("int x = 1++");
            this.AssertThrows<SyntaxErrorException>("int x = 1 +* 2");
            this.AssertThrows<SyntaxErrorException>("int x = 1 << >> 2");
            this.AssertThrows<SyntaxErrorException>("int x = 1 || || 2");
            this.AssertThrows<SyntaxErrorException>("int x = 1 ? 2 : 3 : 2");
            this.AssertThrows<SyntaxErrorException>("int x = f(,);");
        }

        [Test]
        public void InvalidIfStatementTests()
        {
            this.AssertThrows<SyntaxErrorException>("void f () { if (x) }");
            this.AssertThrows<SyntaxErrorException>("void f () { if x {} else {} }");
            this.AssertThrows<SyntaxErrorException>("void f () { if (x) then { } else { } }");
            this.AssertThrows<SyntaxErrorException>("void f () { if (x > 1 {} }");
            this.AssertThrows<SyntaxErrorException>("void f () { if (1) ;; else ; }");
        }

        [Test]
        public void InvalidForStatementTests()
        {
            this.AssertThrows<SyntaxErrorException>("void f () { for (int x, int y; x < y; x++ y++) {} }");
            this.AssertThrows<SyntaxErrorException>("void f () { for (int x, y; x < y; ; x++ y++) {} }");
            this.AssertThrows<SyntaxErrorException>("void f () { for (;;;;){} }");
            this.AssertThrows<SyntaxErrorException>("void f () { for (int i = 0; i < n; i++,) {} }");
        }


        protected override ASTNode GenerateAST(string src)
            => new CASTBuilder().BuildFromSource(src);
    }
}
