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
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Java
{
    internal sealed class InterfaceDeclarationTests : DeclarationTestsBase
    {
        [Test]
        public void EmptyBodyInterfaceDeclTest()
        {
            string src1 = "interface Interface1 {}";
            TypeDeclNode ast1 = this.GenerateAST(src1).As<TypeDeclNode>();

            Assert.That(ast1.Identifier, Is.EqualTo("Interface1"));
            Assert.That(ast1.Declarations.Count, Is.EqualTo(0));
        }

        [Test]
        public void WithTemplateArgsInterfaceDeclTest()
        {
            string src1 = "interface Interface2 <Class1> {}";
            TypeDeclNode ast1 = this.GenerateAST(src1).As<TypeDeclNode>();

            Assert.That(ast1.Identifier, Is.EqualTo("Interface2"));
            Assert.That(ast1.TemplateParameters.Types.First().TypeName, Is.EqualTo("Class1"));
        }


        protected override ASTNode GenerateAST(string src)
            => new JavaASTBuilder().BuildFromSource(src, p => p.interfaceDeclaration());
    }
}
