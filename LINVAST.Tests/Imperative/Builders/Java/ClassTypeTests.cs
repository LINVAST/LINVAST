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
    internal sealed class ClassTypeTests : ASTBuilderTestBase
    {
        [Test]

        public void ClassTypeIdentifierTest()
        {

            TypeDeclNode ast = this.GenerateAST("Point").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("Point"));
            Assert.That(ast.BaseTypes.Children.Count, Is.EqualTo(0));
            Assert.That(ast.TemplateParameters.Children.Count, Is.EqualTo(0));
        }

        [Test]
        public void ClassTypeTypeArgumentsTest()
        {

            TypeDeclNode ast = this.GenerateAST("List<Point>").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("List"));
            Assert.That(ast.BaseTypes.Children.Count, Is.EqualTo(0));
            Assert.That(ast.TemplateParameters.Children.Count, Is.EqualTo(1));
            Assert.That(ast.TemplateParameters.Types.First().Identifier, Is.EqualTo("Point"));

        }

        [Test]
        public void ClassTypeTest()
        {

            TypeDeclNode ast = this.GenerateAST("BaseClass<T>.Class<Template>").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("Class"));
            Assert.That(ast.BaseTypes.Children.Count, Is.EqualTo(1));
            Assert.That(ast.TemplateParameters.Children.Count, Is.EqualTo(1));
            Assert.That(ast.BaseTypes.Types.Count, Is.EqualTo(1));
            Assert.That(ast.BaseTypes.Types.First().Identifier, Is.EqualTo("BaseClass"));
            Assert.That(ast.TemplateParameters.Types.First().Identifier, Is.EqualTo("Template"));

        }
        [Test]
        public void ClassTypeBaseAndArgumentTypesTest()
        {

            TypeDeclNode ast = this.GenerateAST("BaseClass.Class<TemplateClass>").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("Class"));
            Assert.That(ast.BaseTypes.Children.Count, Is.EqualTo(1));
            Assert.That(ast.TemplateParameters.Children.Count, Is.EqualTo(1));
            Assert.That(ast.BaseTypes.Types.First().Identifier, Is.EqualTo("BaseClass"));
            Assert.That(ast.TemplateParameters.Types.First().Identifier, Is.EqualTo("TemplateClass"));

        }

        protected override ASTNode GenerateAST(string src)
        {
            return new JavaASTBuilder().BuildFromSource(src, p => p.classType());

        }
    }
}