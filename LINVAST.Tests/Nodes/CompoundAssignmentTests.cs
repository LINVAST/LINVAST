using System.Linq;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class CompoundAssignmentTests
    {
        [TestCase("+=", 7)]
        [TestCase("-=", 1)]
        [TestCase("*=", 12)]
        [TestCase("/=", 1)]
        [TestCase("%=", 1)]
        [TestCase("&=", 0)]
        [TestCase("|=", 7)]
        [TestCase("^=", 7)]
        [TestCase("<<=", 32)]
        [TestCase(">>=", 0)]
        public void ExpansionPreservesOperatorAndIndependentOwnership(string symbol, int expected)
        {
            var original = Parse($"x {symbol} 3");
            string before = original.ToJson();
            var expanded = original.SimplifyComplexAssignment();
            Assert.That(expanded.Operator.Symbol, Is.EqualTo("="));
            var binary = expanded.RightOperand.As<ArithmExprNode>();
            Assert.That(binary.LeftOperand, Is.Not.SameAs(expanded.LeftOperand));
            Assert.That(binary.Operator.ApplyTo, Is.SameAs(original.Operator.ApplyTo));
            var evaluable = binary.Substitute(new IdNode(1, "x"), new LitExprNode(1, 4)).As<ExprNode>();
            Assert.That(ConstantExpressionEvaluator.Evaluate(evaluable), Is.EqualTo(expected));
            Assert.That(original.ToJson(), Is.EqualTo(before));
            LoopInvariantTests.AssertOwnedTree(original);
            LoopInvariantTests.AssertOwnedTree(expanded);
        }

        [TestCase("a[i++] += f()")]
        [TestCase("*p += f()")]
        [TestCase("p->x += f()")]
        [TestCase("a[0] += f()")]
        public void ComplexTargetsRemainCompoundAndAreCopied(string source)
        {
            var original = Parse(source);
            var result = original.SimplifyComplexAssignment();
            Assert.That(result, Is.EqualTo(original));
            Assert.That(result, Is.Not.SameAs(original));
            Assert.That(result.LeftOperand, Is.Not.SameAs(original.LeftOperand));
            Assert.That(result.Operator.Symbol, Is.EqualTo("+="));
            Assert.That(Descendants(result).OfType<FuncCallExprNode>().Count(), Is.EqualTo(1));
            LoopInvariantTests.AssertOwnedTree(result);
            LoopInvariantTests.AssertOwnedTree(original);
        }

        [Test]
        public void SimpleAssignmentIsAlsoAnIndependentCopy()
        {
            var original = Parse("x = 1");
            var result = original.SimplifyComplexAssignment();
            Assert.That(result, Is.EqualTo(original));
            Assert.That(result, Is.Not.SameAs(original));
            LoopInvariantTests.AssertOwnedTree(result);
        }

        private static AssignExprNode Parse(string code)
            => new CASTBuilder().BuildFromSource(code, p => p.expression()).As<AssignExprNode>();

        private static System.Collections.Generic.IEnumerable<ASTNode> Descendants(ASTNode node)
            => node.Children.SelectMany(c => new[] { c }.Concat(Descendants(c)));
    }
}
