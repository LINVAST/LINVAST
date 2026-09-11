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
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Java
{
    internal sealed class WildCardTests : ASTBuilderTestBase
    {
        [Test]

        public void nonWildcardTypeArgumentsTypeTypeTest()
        {
            TypeNameListNode ast = this.GenerateAST("<int>").As<TypeNameListNode>();

            Assert.That(ast.Types.Count, Is.EqualTo(1));
            Assert.That(ast.Types.First().Identifier, Is.EqualTo("int"));
        }

        [Test]

        public void nonWildcardTypeArgumentsTypeListTest()
        {
            TypeNameListNode ast = this.GenerateAST("<String, Point>").As<TypeNameListNode>();

            Assert.That(ast.Types.Count, Is.EqualTo(2));
            Assert.That(ast.Types.First().Identifier, Is.EqualTo("String"));
            Assert.That(ast.Types.Last().Identifier, Is.EqualTo("Point"));
        }

        protected override ASTNode GenerateAST(string src)
        {
            return new JavaASTBuilder().BuildFromSource(src, p => p.nonWildcardTypeArgumentsOrDiamond());
        }
    }
}