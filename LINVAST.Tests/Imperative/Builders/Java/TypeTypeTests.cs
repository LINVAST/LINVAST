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
    internal sealed class TypeTypesTests : ASTBuilderTestBase
    {
        [Test]

        public void PrimitiveTypeTest()
        {

            TypeNameNode ast = this.GenerateAST("int").As<TypeNameNode>();


            Assert.That(ast.Identifier, Is.EqualTo("int"));
            Assert.That(ast.TemplateArguments.Count, Is.EqualTo(0));
        }

        [Test]
        public void VoidTypeTest()
        {
            TypeNameNode ast = this.GenerateAST("void").As<TypeNameNode>();

            Assert.That(ast.Identifier, Is.EqualTo("void"));
        }

        [Test]

        public void ClassOrInterfaceTypeTest()
        {

            TypeNameNode ast = this.GenerateAST("Point").As<TypeNameNode>();

            Assert.That(ast.Identifier, Is.EqualTo("Point"));
            Assert.That(ast.TemplateArguments.Count, Is.EqualTo(0));
        }

        [Test]
        public void ClassOrInterfaceTypeWithTypeArgumentTest()
        {

            TypeNameNode ast = this.GenerateAST("ArrayList<double>").As<TypeNameNode>();

            Assert.That(ast.Identifier, Is.EqualTo("ArrayList"));
            Assert.That(ast.TemplateArguments.Count, Is.EqualTo(1));
            Assert.That(ast.TemplateArguments.First().Identifier, Is.EqualTo("double"));
        }

        [Test]
        public void ClassOrInterfaceTypeWithTypeArgumentsTest()
        {

            TypeNameNode ast = this.GenerateAST("Map<String, Point>").As<TypeNameNode>();

            Assert.That(ast.Identifier, Is.EqualTo("Map"));
            Assert.That(ast.TemplateArguments.Count, Is.EqualTo(2));
            Assert.That(ast.TemplateArguments.First().Identifier, Is.EqualTo("String"));
            Assert.That(ast.TemplateArguments.Last().Identifier, Is.EqualTo("Point"));
        }


        [Test]
        public void ClassOrInterfaceTypeWithClassOrInterfaceTest()
        {

            TypeNameNode ast = this.GenerateAST("Map<String, Point<int, int>>").As<TypeNameNode>();

            Assert.That(ast.Identifier, Is.EqualTo("Map"));
            Assert.That(ast.TemplateArguments.Count, Is.EqualTo(2));
            Assert.That(ast.TemplateArguments.First().Identifier, Is.EqualTo("String"));
            Assert.That(ast.TemplateArguments.Last().Identifier, Is.EqualTo("Point"));
            Assert.That(ast.TemplateArguments.Last().TemplateArguments.Count, Is.EqualTo(2));
            Assert.That(ast.TemplateArguments.Last().TemplateArguments.First().Identifier, Is.EqualTo("int"));
            Assert.That(ast.TemplateArguments.Last().TemplateArguments.Last().Identifier, Is.EqualTo("int"));
        }
        protected override ASTNode GenerateAST(string src)
        {
            return new JavaASTBuilder().BuildFromSource(src, p => p.typeTypeOrVoid());
        }
    }
}