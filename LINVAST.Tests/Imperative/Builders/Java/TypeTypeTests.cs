using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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

            TypeDeclNode ast = this.GenerateAST("A").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("A"));
        }

        [Test]
        public void ClassOrInterfaceTypeWithTypeArgumentTest()
        {

            TypeDeclNode ast = this.GenerateAST("A<B>").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("A"));
            Assert.That(ast.TemplateParameters.Identifiers.First().Identifier, Is.EqualTo("B"));
        }

        [Test]
        public void ClassOrInterfaceTypeWithTypeArgumentsTest()
        {

            TypeDeclNode ast = this.GenerateAST("A<B, C>").As<TypeDeclNode>();

            Assert.That(ast.Identifier, Is.EqualTo("A"));
            Assert.That(ast.TemplateParameters.Identifiers.First().Identifier, Is.EqualTo("B"));
            Assert.That(ast.TemplateParameters.Identifiers.Last().Identifier, Is.EqualTo("C"));
        }

        protected override ASTNode GenerateAST(string src)
        {
            return new JavaASTBuilder().BuildFromSource(src, p => p.typeTypeOrVoid());
        }
    }
}
