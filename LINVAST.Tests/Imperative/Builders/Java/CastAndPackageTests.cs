using System.Linq;
using LINVAST.Exceptions;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using LINVAST.Tests.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Java
{
    internal sealed class CastAndPackageTests
    {
        [Test]
        public void CompilationUnitPreservesPackageBeforeImportsAndTypes()
        {
            var builder = new JavaASTBuilder();
            var root = builder.BuildFromSource("package a; import java.util.List; class C {}");
            Assert.That(root.Children[0].As<PackageNode>().Identifier, Is.EqualTo("a"));
            Assert.That(root.Children.Count, Is.EqualTo(3));
            Assert.That(root, Is.Not.EqualTo(builder.BuildFromSource("package b; import java.util.List; class C {}")));
            LoopInvariantTests.AssertOwnedTree(root);
        }

        [TestCase("(int)1.5", "int")]
        [TestCase("(String)value", "string")]
        [TestCase("(int[])values", "int[]")]
        [TestCase("(java.util.List<String>)values", "java.util.List")]
        public void CastPreservesItsTypeAndOperand(string source, string expectedType)
        {
            var expr = new JavaASTBuilder().BuildFromSource(source, p => p.expression()).As<ExprNode>();
            Assert.That(expr.NodeType, Is.EqualTo("CastExprNode"));
            Assert.That(expr.Children[0].As<TypeNameNode>().TypeName, Is.EqualTo(expectedType));
            Assert.That(expr.Children[1], Is.InstanceOf<ExprNode>());
            Assert.Throws<EvaluationException>(() => ConstantExpressionEvaluator.Evaluate(expr));
            Assert.Throws<EvaluationException>(() => new SymbolicExpressionBuilder(expr).Parse());
            LoopInvariantTests.AssertOwnedTree(expr);
            Assert.That(expr.Copy(), Is.EqualTo(expr));
        }

        [Test]
        public void CastsInsideWholeFilesAndNestedCastsAreRetained()
        {
            var root = new JavaASTBuilder().BuildFromSource("class C { int f() { return (int)(long)1.5; } }");
            int Count(LINVAST.Nodes.ASTNode node) => (node.NodeType == "CastExprNode" ? 1 : 0) + node.Children.Sum(Count);
            Assert.That(Count(root), Is.EqualTo(2));
            LoopInvariantTests.AssertOwnedTree(root);
        }
    }
}
