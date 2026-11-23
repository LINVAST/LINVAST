using System;
using System.Collections.Generic;
using System.Linq;
using LINVAST.Exceptions;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Builders.Go;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Builders.Lua;
using LINVAST.Imperative.Builders.Python;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Imperative.Visitors;
using LINVAST.Nodes;
using LINVAST.Tests.Nodes;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace LINVAST.Tests.Visitors
{
    internal sealed class ExpressionSemanticsTests
    {
        [TestCase("Python", "3 / 2", 1.5)]
        [TestCase("Lua", "3 / 2", 1.5)]
        [TestCase("C", "3 / 2", 1)]
        [TestCase("Java", "3 / 2", 1)]
        [TestCase("Kotlin", "3 / 2", 1)]
        [TestCase("Go", "3 / 2", 1)]
        [TestCase("Java", "3.0 / 2", 1.5)]
        [TestCase("Python", "-3 // 2", -2)]
        [TestCase("Python", "3 // -2", -2)]
        [TestCase("Python", "-3 // -2", 1)]
        [TestCase("Python", "1.0 // 0.1", 9.0)]
        [TestCase("Lua", "-3 // 2", -2)]
        [TestCase("Python", "-3 % 2", 1)]
        [TestCase("Python", "3 % -2", -1)]
        [TestCase("Python", "-3.5 % 2", 0.5)]
        [TestCase("Lua", "-3 % 2", 1)]
        [TestCase("Lua", "3 % -2", -1)]
        [TestCase("C", "-3 % 2", -1)]
        [TestCase("Java", "-3 % 2", -1)]
        [TestCase("Java", "3 % -2", 1)]
        [TestCase("Kotlin", "-3 % 2", -1)]
        [TestCase("Go", "-3 % 2", -1)]
        [TestCase("Python", "0 or 5", 5)]
        [TestCase("Python", "0 and (1 / 0)", 0)]
        [TestCase("Python", "'hello' or unknown()", "hello")]
        [TestCase("Python", "'' or 'fallback'", "fallback")]
        [TestCase("Python", "1 and 2 and 3", 3)]
        [TestCase("Python", "None or 5", 5)]
        [TestCase("Python", "not None", true)]
        [TestCase("Python", "not ''", true)]
        [TestCase("Python", "7 if '' else 9", 9)]
        [TestCase("Python", "7 if 'x' else unknown()", 7)]
        [TestCase("Lua", "0 or unknown()", 0)]
        [TestCase("Lua", "'' or unknown()", "")]
        [TestCase("Lua", "nil or 5", 5)]
        [TestCase("Lua", "not 0", false)]
        [TestCase("Lua", "not ''", false)]
        [TestCase("Lua", "false and unknown()", false)]
        [TestCase("Lua", "0 and 5", 5)]
        [TestCase("C", "2 ? 3 : unknown()", 3)]
        [TestCase("Java", "false && unknown()", false)]
        [TestCase("Java", "true || unknown()", true)]
        public void BuildersPreserveOperationBehavior(string language, string source, object expected)
        {
            ExprNode expr = Parse(language, source);
            Assert.That(ConstantExpressionEvaluator.Evaluate(expr), Is.EqualTo(expected));
            LoopInvariantTests.AssertOwnedTree(expr);
        }

        [TestCase("Java", "-1 >>> 1", 2147483647L)]
        [TestCase("Java", "-1L >>> 1", 9223372036854775807L)]
        [TestCase("Java", "-1 >>> 32", -1L)]
        [TestCase("Java", "-1L >>> 64", -1L)]
        [TestCase("Java", "-1 >>> -1", 1L)]
        [TestCase("Java", "-1 >>> 1L", 2147483647L)]
        [TestCase("Java", "-8 >> 2", -2L)]
        [TestCase("Java", "1 << 33", 2L)]
        [TestCase("Python", "-8 >> 2", -2L)]
        [TestCase("Python", "8 >> 64", 0L)]
        [TestCase("Python", "-8 >> 64", -1L)]
        [TestCase("Python", "1 << 40", 1099511627776L)]
        [TestCase("Lua", "-1 >> 1", 9223372036854775807L)]
        [TestCase("Lua", "8 >> -1", 16L)]
        [TestCase("Lua", "8 << -1", 4L)]
        [TestCase("Lua", "8 >> 64", 0L)]
        [TestCase("Lua", "8 << 64", 0L)]
        public void ShiftRulesUseTheCorrectWidthAndCount(string language, string source, long expected)
            => Assert.That(ConstantExpressionEvaluator.Evaluate(Parse(language, source)), Is.EqualTo(expected));

        [TestCase("Python", "1 / 0")]
        [TestCase("Python", "1.0 // 0.0")]
        [TestCase("Python", "1.0 % 0.0")]
        [TestCase("Java", "1 / 0")]
        [TestCase("Lua", "1 // 0")]
        public void RequiredZeroDivisionIsNotSwallowed(string language, string source)
            => Assert.Throws<DivideByZeroException>(() => ConstantExpressionEvaluator.Evaluate(Parse(language, source)));

        [TestCase("Java", "1 && true")]
        [TestCase("Java", "!0")]
        [TestCase("Kotlin", "1 || false")]
        [TestCase("Go", "1 && true")]
        [TestCase("Python", "1 >> -1")]
        [TestCase("Python", "1 << 1000")]
        [TestCase("C", "-1 >> 1")]
        [TestCase("C", "1 << 32")]
        public void UnsupportedOrInvalidSemanticsAreExplicit(string language, string source)
            => Assert.Throws<EvaluationException>(() => ConstantExpressionEvaluator.Evaluate(Parse(language, source)));

        [TestCase("Python", "None and unknown()")]
        [TestCase("Lua", "nil and unknown()")]
        public void ShortCircuitCanReturnNull(string language, string source)
            => Assert.That(ConstantExpressionEvaluator.Evaluate(Parse(language, source)), Is.Null);

        [TestCase("Python", "0 and (1 / 0)", "0")]
        [TestCase("Lua", "0 or unknown()", "0")]
        [TestCase("Java", "3 / 2", "1")]
        [TestCase("Python", "3 / 2", "3/2")]
        [TestCase("Java", "-1 >>> 1", "2147483647")]
        public void SymbolicConstantsRespectSemanticsAndShortCircuit(string language, string source, string expected)
            => Assert.That(new SymbolicExpressionBuilder(Parse(language, source)).Parse().ToString(), Is.EqualTo(expected));

        [Test]
        public void SymbolicUnknownOperationsKeepTheirDistinctSemantics()
        {
            string Convert(string lang) => new SymbolicExpressionBuilder(Parse(lang, "x / 2")).Parse().ToString();
            Assert.That(Convert("Python"), Is.EqualTo(Convert("Java")));
            Assert.That(Convert("Java"), Is.EqualTo("x/2"));
            Assert.That(Convert("Java"), Is.EqualTo(Convert("Java")));
            var calls = Parse("Python", "(f() / 2) - (f() / 2)");
            Assert.That(new SymbolicExpressionBuilder(calls).Parse().ToString(), Is.Not.EqualTo("0"));
        }

        [Test]
        public void MetadataParticipatesInEqualityCopySubstitutionAndJson()
        {
            ArithmExprNode Make(DivisionMode mode) => new(1, new IdNode(1, "x"), ArithmOpNode.FromSymbol(1, "/"), new LitExprNode(1, 2)) {
                Semantics = new(division: mode)
            };
            var trueDivision = Make(DivisionMode.True);
            var integerDivision = Make(DivisionMode.TruncateIntegers);
            Assert.That(trueDivision, Is.Not.EqualTo(integerDivision));
            Assert.That(new HashSet<ASTNode> { trueDivision, integerDivision, trueDivision.Copy() }.Count, Is.EqualTo(2));
            var copy = trueDivision.Copy().As<ArithmExprNode>();
            Assert.That(copy.Semantics, Is.SameAs(trueDivision.Semantics));
            Assert.That(copy.GetHashCode(), Is.EqualTo(trueDivision.GetHashCode()));
            var substituted = trueDivision.Substitute(trueDivision.LeftOperand, new LitExprNode(1, 3)).As<ArithmExprNode>();
            Assert.That(substituted.Semantics, Is.EqualTo(trueDivision.Semantics));
            Assert.That(ConstantExpressionEvaluator.Evaluate(substituted), Is.EqualTo(1.5));
            Assert.That(JObject.Parse(copy.ToJson())["Semantics"]!["Division"]!.Value<string>(), Is.EqualTo("True"));
            Assert.That(copy.Children.Count, Is.EqualTo(3));
            LoopInvariantTests.AssertOwnedTree(trueDivision);
            LoopInvariantTests.AssertOwnedTree(substituted);
            Assert.That(JsonConvert.DeserializeObject<ExpressionSemantics>(JsonConvert.SerializeObject(copy.Semantics)), Is.EqualTo(copy.Semantics));
        }

        [TestCase("/=", 3, 1.5)]
        [TestCase("//=", -3, -2)]
        [TestCase("%=", -3, 1)]
        public void CompoundAssignmentMovesMetadataToTheExpandedOperation(string op, int value, object expected)
        {
            var tree = new PythonASTBuilder().BuildFromSource($"x = 0\nx {op} 2\n");
            var original = Descendants(tree).OfType<AssignExprNode>().Single();
            var expanded = original.SimplifyComplexAssignment();
            Assert.That(expanded.Semantics, Is.Null);
            Assert.That(expanded.RightOperand.Semantics, Is.EqualTo(original.Semantics));
            var result = expanded.RightOperand.Substitute(new IdNode(1, "x"), new LitExprNode(1, value)).As<ExprNode>();
            Assert.That(ConstantExpressionEvaluator.Evaluate(result), Is.EqualTo(expected));
            LoopInvariantTests.AssertOwnedTree(tree);
            LoopInvariantTests.AssertOwnedTree(expanded);
        }

        [Test]
        public void ComplexFloorAssignmentRetainsOneTargetEvaluation()
        {
            var tree = new PythonASTBuilder().BuildFromSource("a[f()] //= 2\n");
            var original = Descendants(tree).OfType<AssignExprNode>().Single();
            var simplified = original.SimplifyComplexAssignment();
            Assert.That(simplified.Operator.Symbol, Is.EqualTo("//="));
            Assert.That(simplified.Semantics!.Division, Is.EqualTo(DivisionMode.Floor));
            Assert.That(Descendants(simplified).OfType<FuncCallExprNode>().Count(), Is.EqualTo(1));
            LoopInvariantTests.AssertOwnedTree(tree);
            LoopInvariantTests.AssertOwnedTree(simplified);
        }

        [TestCase("Python", "x = 3 / 2\n", DivisionMode.True)]
        [TestCase("Lua", "local x = 3 / 2", DivisionMode.True)]
        [TestCase("Java", "class C { double x = 3 / 2; }", DivisionMode.TruncateIntegers)]
        [TestCase("C", "int x = 3 / 2;", DivisionMode.TruncateIntegers)]
        public void WholeFilesKeepMetadata(string lang, string source, DivisionMode expected)
        {
            ASTNode tree = lang switch {
                "Python" => new PythonASTBuilder().BuildFromSource(source),
                "Lua" => new LuaASTBuilder().BuildFromSource(source),
                "Java" => new JavaASTBuilder().BuildFromSource(source),
                _ => new CASTBuilder().BuildFromSource(source),
            };
            Assert.That(Descendants(tree).OfType<ArithmExprNode>().Single().Semantics!.Division, Is.EqualTo(expected));
            LoopInvariantTests.AssertOwnedTree(tree);
        }

        [Test]
        public void InvalidMetadataCombinationsAreRejected()
        {
            Assert.Throws<ArgumentException>(() => new ExpressionSemantics(division: DivisionMode.True, truthiness: TruthinessMode.Python));
            Assert.Throws<ArgumentException>(() => new ExpressionSemantics(logicalResult: LogicalResultMode.SelectedOperand));
            Assert.Throws<ArgumentException>(() => new ExpressionSemantics(rightShift: RightShiftMode.Logical));
            Assert.Throws<ArgumentOutOfRangeException>(() => new ExpressionSemantics(integerWidth: 16));
            Assert.Throws<ArgumentOutOfRangeException>(() => new ExpressionSemantics(division: (DivisionMode)99));
            var invalid = new ArithmExprNode(1, new LitExprNode(1, 1), ArithmOpNode.FromSymbol(1, "+"), new LitExprNode(1, 2)) {
                Semantics = new(division: DivisionMode.True)
            };
            Assert.Throws<EvaluationException>(() => ConstantExpressionEvaluator.Evaluate(invalid));
            Assert.Throws<EvaluationException>(() => new SymbolicExpressionBuilder(invalid).Parse());
        }

        [Test]
        public void ManualExpressionsRetainLegacyBehavior()
        {
            var expr = new ArithmExprNode(1, new LitExprNode(1, 3), ArithmOpNode.FromSymbol(1, "/"), new LitExprNode(1, 2));
            Assert.That(expr.Semantics, Is.Null);
            Assert.That(ConstantExpressionEvaluator.Evaluate(expr), Is.EqualTo(1));
            Assert.That(JObject.Parse(expr.ToJson())["Semantics"], Is.Null);
        }

        private static IEnumerable<ASTNode> Descendants(ASTNode node)
            => new[] { node }.Concat(node.Children.SelectMany(Descendants));

        private static ExprNode Parse(string lang, string source) => (lang switch {
            "Python" => new PythonASTBuilder().BuildFromSource(source, p => p.test()),
            "Lua" => new LuaASTBuilder().BuildFromSource(source, p => p.exp()),
            "Java" => new JavaASTBuilder().BuildFromSource(source, p => p.expression()),
            "Kotlin" => new KotlinASTBuilder().BuildFromSource(source, p => p.expression()),
            "Go" => new GoASTBuilder().BuildFromSource(source, p => p.expression()),
            _ => new CASTBuilder().BuildFromSource(source, p => p.expression()),
        }).As<ExprNode>();
    }
}
