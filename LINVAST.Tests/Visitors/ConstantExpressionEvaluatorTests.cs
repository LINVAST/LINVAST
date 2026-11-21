using System;
using LINVAST.Exceptions;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using NUnit.Framework;

namespace LINVAST.Tests.Visitors
{
    internal sealed class ConstantExpressionEvaluatorTests
    {
        [TestCase("1 == 1 ? 42 : 1 / 0", 42)]
        [TestCase("1 == 2 ? 1 / 0 : 42", 42)]
        [TestCase("1 == 1 ? 42 : unknown", 42)]
        [TestCase("1 == 2 ? unknown : 42", 42)]
        [TestCase("0 && (1 / 0)", false)]
        [TestCase("1 || (1 / 0)", true)]
        [TestCase("0 && unknown", false)]
        [TestCase("1 || unknown", true)]
        [TestCase("1 && 2", true)]
        [TestCase("0 || 0", false)]
        public void EvaluatesOnlyRequiredOperands(string source, object expected)
            => Assert.That(ConstantExpressionEvaluator.Evaluate(Parse(source)), Is.EqualTo(expected));

        [TestCase("1 == 1 ? 1 / 0 : 42")]
        [TestCase("1 == 2 ? 42 : 1 / 0")]
        [TestCase("1 && (1 / 0)")]
        [TestCase("0 || (1 / 0)")]
        public void RequiredOperandsStillRaiseErrors(string source)
            => Assert.Throws<DivideByZeroException>(() => ConstantExpressionEvaluator.Evaluate(Parse(source)));

        [TestCase("and", false)]
        [TestCase("or", true)]
        public void LogicalAliasesShortCircuit(string symbol, bool left)
        {
            var expr = new LogicExprNode(1, new LitExprNode(1, left),
                BinaryLogicOpNode.FromSymbol(1, symbol), new IdNode(1, "unknown"));
            Assert.That(ConstantExpressionEvaluator.Evaluate(expr), Is.EqualTo(left));
        }

        [Test]
        public void InvalidConditionIsReportedBeforeVisitingBranches()
        {
            var expr = new CondExprNode(1, new LitExprNode(1, "invalid"),
                Parse("1 / 0"), Parse("1 / 0"));
            Assert.Throws<EvaluationException>(() => ConstantExpressionEvaluator.Evaluate(expr));
        }

        private static ExprNode Parse(string source)
            => new CASTBuilder().BuildFromSource(source, p => p.expression()).As<ExprNode>();
    }
}
