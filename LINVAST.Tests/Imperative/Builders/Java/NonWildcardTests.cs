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
    internal sealed class WildCardTests : ASTBuilderTestBase
    {
        [Test]

        public void nonWildcardTypeArgumentsTypeTypeTest()
        {
            TypeNameListNode ast = GenerateAST("<int>").As<TypeNameListNode>();

            Assert.That(ast.Types.Count, Is.EqualTo(1));
            Assert.That(ast.Types.First().Identifier, Is.EqualTo("int"));
        }

        [Test]

        public void nonWildcardTypeArgumentsTypeListTest()
        {
            TypeNameListNode ast = GenerateAST("<String, Point>").As<TypeNameListNode>();

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