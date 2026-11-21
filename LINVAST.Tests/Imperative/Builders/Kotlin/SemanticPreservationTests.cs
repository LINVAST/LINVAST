using System.Linq;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Imperative.Visitors;
using LINVAST.Tests.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class SemanticPreservationTests
    {
        [TestCase("!!true", true)]
        [TestCase("!!!true", false)]
        [TestCase("- - 2", 2)]
        [TestCase("+ - 2", -2)]
        public void AllPrefixOperatorsArePreservedInOrder(string source, object expected)
        {
            var expr = new KotlinASTBuilder().BuildFromSource(source, p => p.expression()).As<ExprNode>();
            Assert.That(ConstantExpressionEvaluator.Evaluate(expr), Is.EqualTo(expected));
            LoopInvariantTests.AssertOwnedTree(expr);
        }

        [Test]
        public void ExpressionFunctionKeepsDefaultAndReturnValue()
        {
            var root = new KotlinASTBuilder().BuildFromSource("fun f(x: Int = 42) = x");
            var function = root.Children.Single().As<FuncNode>();
            Assert.That(function.ReturnTypeName, Is.EqualTo("var"));
            var param = function.Parameters!.Single().Declarator.As<VarDeclNode>();
            Assert.That(param.Initializer!.As<LitExprNode>().Value, Is.EqualTo(42));
            var ret = function.Definition!.Children.Single().As<JumpStatNode>();
            Assert.That(ret.Type, Is.EqualTo(JumpStatType.Return));
            Assert.That(ret.ReturnExpr!.As<IdNode>().Identifier, Is.EqualTo("x"));
            LoopInvariantTests.AssertOwnedTree(root);
        }

        [Test]
        public void NonConstantDefaultValueAndExplicitReturnTypeArePreserved()
        {
            var function = new KotlinASTBuilder().BuildFromSource("fun f(x: Int = getDefault()): Int = x")
                .Children.Single().As<FuncNode>();
            Assert.That(function.ReturnTypeName, Is.EqualTo("int"));
            Assert.That(function.Parameters!.Single().Declarator.As<VarDeclNode>().Initializer!.As<FuncCallExprNode>().Identifier,
                Is.EqualTo("getDefault"));
            Assert.That(function.Definition!.Children.Single(), Is.TypeOf<JumpStatNode>());
        }
    }
}
