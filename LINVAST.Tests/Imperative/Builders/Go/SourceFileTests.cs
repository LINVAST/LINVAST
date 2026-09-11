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


using System.Linq;
using LINVAST.Imperative.Builders.Go;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Go
{
    internal sealed class SourceFileTests : ASTBuilderTestBase
    {
        [Test]
        public void SourceFileIncludesPackageNodeTest()
        {
            SourceNode source = this.GenerateAST("package demo; var x int").As<SourceNode>();

            PackageNode package = source.Children.First().As<PackageNode>();
            Assert.That(package.Identifier, Is.EqualTo("demo"));
            Assert.That(source.Children.Last(), Is.InstanceOf<DeclStatNode>());
        }

        [Test]
        public void TypeDeclarationTest()
        {
            DeclStatNode typeDecl = this.GenerateAST("package demo; type Point struct { x int }")
                .As<SourceNode>()
                .Children
                .Last()
                .As<DeclStatNode>();

            Assert.That(typeDecl.Specifiers.TypeName, Is.EqualTo("struct{xint}"));
            Assert.That(typeDecl.DeclaratorList.Declarators.Single().Identifier, Is.EqualTo("Point"));
        }

        [Test]
        public void MethodDeclarationWithParametersKeepsBodyTest()
        {
            FuncNode method = this.GenerateAST("package demo; func (p Point) Move(dx int) { return }")
                .As<SourceNode>()
                .Children
                .Last()
                .As<FuncNode>();

            Assert.That(method.Identifier, Is.EqualTo("Point.Move"));
            Assert.That(method.Parameters, Has.Exactly(2).Items);
            Assert.That(method.Parameters!.Select(p => (p.Specifiers.TypeName, p.Declarator.Identifier)),
                Is.EqualTo(new[] { ("Point", "p"), ("int", "dx") }));
            Assert.That(method.Definition, Is.Not.Null);
            Assert.That(method.Definition!.Children.Single(), Is.InstanceOf<JumpStatNode>());
        }

        protected override ASTNode GenerateAST(string src)
            => new GoASTBuilder().BuildFromSource(src);
    }
}
