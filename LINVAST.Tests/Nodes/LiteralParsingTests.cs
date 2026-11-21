using System.Globalization;
using LINVAST.Exceptions;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class LiteralParsingTests
    {
        [TestCase("en-US")]
        [TestCase("de-DE")]
        [TestCase("fr-FR")]
        public void NumericLiteralsUseInvariantCulture(string culture)
        {
            CultureInfo previous = CultureInfo.CurrentCulture;
            try {
                CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo(culture);
                Assert.That(LitExprNode.FromString(1, "1.5").Value, Is.EqualTo(1.5));
                Assert.That(LitExprNode.FromString(1, "1.5f").Value, Is.EqualTo(1.5f));
                Assert.That(LitExprNode.FromString(1, "1.5m").Value, Is.EqualTo(1.5m));
                var expr = new CASTBuilder().BuildFromSource("1.5 + 2.5", p => p.expression()).As<ExprNode>();
                Assert.That(ConstantExpressionEvaluator.Evaluate(expr), Is.EqualTo(4.0));
                Assert.That(new SymbolicExpressionBuilder(expr).Parse().ToString(), Is.EqualTo("4"));
            } finally {
                CultureInfo.CurrentCulture = previous;
            }
        }

        [TestCase("1.", 1.0)]
        [TestCase(".5", 0.5)]
        [TestCase("1.e2", 100.0)]
        [TestCase("1.E-2", 0.01)]
        [TestCase("1e2", 100.0)]
        public void CDecimalFloatingForms(string source, double expected)
        {
            var literal = new CASTBuilder().BuildFromSource(source, p => p.expression()).As<LitExprNode>();
            Assert.That(literal.Value, Is.EqualTo(expected));
        }

        [Test]
        public void UnsuffixedIntegersWidenAtBoundaries()
        {
            AssertLiteral("2147483647", int.MaxValue);
            AssertLiteral("2147483648", 2147483648U);
            AssertLiteral("4294967295", uint.MaxValue);
            AssertLiteral("4294967296", 4294967296L);
            AssertLiteral("9223372036854775807", long.MaxValue);
            AssertLiteral("9223372036854775808", 9223372036854775808UL);
            AssertLiteral("18446744073709551615", ulong.MaxValue);
            AssertLiteral("0x100000000", 4294967296L);
        }

        [TestCase("18446744073709551616")]
        [TestCase("4294967296U")]
        [TestCase("9223372036854775808L")]
        [TestCase("18446744073709551616UL")]
        [TestCase("0x10000000000000000")]
        [TestCase("1e999")]
        [TestCase("1e99F")]
        [TestCase("1e99M")]
        public void OutOfRangeLiteralsHaveSyntaxDiagnostics(string source)
            => Assert.Throws<SyntaxErrorException>(() => LitExprNode.FromString(1, source));

        private static void AssertLiteral<T>(string source, T expected)
        {
            object? value = LitExprNode.FromString(1, source).Value;
            Assert.That(value, Is.TypeOf<T>());
            Assert.That(value, Is.EqualTo(expected));
        }
    }
}
