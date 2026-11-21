using System.Linq;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Visitors;
using NUnit.Framework;

namespace LINVAST.Tests.Visitors
{
    internal sealed class SymbolicFunctionCallTests
    {
        [TestCase("f(1) - f(2)")]
        [TestCase("f(1) - f(1)")]
        [TestCase("f() - f()")]
        [TestCase("a.f(1) - b.f(1)")]
        [TestCase("g(f(1)) - g(f(2))")]
        public void CallsDoNotCancelWithoutAPurityGuarantee(string source)
        {
            var node = new CASTBuilder().BuildFromSource(source, p => p.expression());
            var result = new SymbolicExpressionBuilder(node).Parse();
            Assert.That(result.ToString(), Is.Not.EqualTo("0"));
            Assert.That(result.CollectVariables().Count(), Is.EqualTo(2));
        }

        [Test]
        public void IndependentEvaluationsOfTheSameCallAreDistinct()
        {
            var node = new CASTBuilder().BuildFromSource("f(1)", p => p.expression());
            var builder = new SymbolicExpressionBuilder(node);
            Assert.That(builder.Parse().ToString(), Is.Not.EqualTo(builder.Parse().ToString()));
        }
    }
}
