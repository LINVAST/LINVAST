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
    internal sealed class TypeParametersTests : ASTBuilderTestBase
    {

        [Test]

        public void TypeParameterTest()
        {
            TypeNameListNode ast = this.GenerateAST("< A >").As<TypeNameListNode>();

            Assert.That(ast.Types.First().Identifier, Is.EqualTo("A"));
        }

        [Test]

        public void TypeParametersTest()
        {
            TypeNameListNode ast = this.GenerateAST("< A, B >").As<TypeNameListNode>();

            Assert.That(ast.Types.First().Identifier, Is.EqualTo("A"));
            Assert.That(ast.Types.Last().Identifier, Is.EqualTo("B"));
        }

        [Test]
        public void TypeParameterWithExtendsTest()
        {

            TypeNameListNode ast = this.GenerateAST("< A extends B>").As<TypeNameListNode>();

            Assert.That(ast.Types.First().Identifier, Is.EqualTo("A"));
            Assert.That(ast.Types.First().BaseTypes.Identifiers.First().Identifier, Is.EqualTo("B"));
        }
        [Test]
        public void TypeParametersWithExtendsTest()
        {

            TypeNameListNode ast = this.GenerateAST("< A extends C, B>").As<TypeNameListNode>();

            Assert.That(ast.Types.First().Identifier, Is.EqualTo("A"));
            Assert.That(ast.Types.First().BaseTypes.Identifiers.First().Identifier, Is.EqualTo("C"));
            Assert.That(ast.Types.Last().Identifier, Is.EqualTo("B"));
        }

        [Test]
        public void TypeParametersWithExtendsMultipleTest()
        {

            TypeNameListNode ast = this.GenerateAST("< A extends C & D, B>").As<TypeNameListNode>();

            Assert.That(ast.Types.First().Identifier, Is.EqualTo("A"));
            Assert.That(ast.Types.First().BaseTypes.Identifiers.First().Identifier, Is.EqualTo("C"));
            Assert.That(ast.Types.First().BaseTypes.Identifiers.Last().Identifier, Is.EqualTo("D"));
            Assert.That(ast.Types.Last().Identifier, Is.EqualTo("B"));
        }
        protected override ASTNode GenerateAST(string src)
        {
            return new JavaASTBuilder().BuildFromSource(src, p => p.typeParameters());

        }

    }
}