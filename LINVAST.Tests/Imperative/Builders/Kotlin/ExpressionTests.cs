using System;
using System.Collections.Generic;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class ExpressionTests : ASTBuilderTestBase
    {
        protected override ASTNode GenerateAST(string src)
            => new KotlinASTBuilder().BuildFromSource(src, p => p.expression());

        // --- literalConstant ---
        // These test VisitLiteralConstant in isolation; no other visitor needed.

        [Test]
        public void IntegerLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.literalConstant())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
            Assert.That(node.TypeCode, Is.EqualTo(System.TypeCode.Int32));
        }

        [Test]
        public void BoolTrueLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("true", p => p.literalConstant())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(true));
            Assert.That(node.TypeCode, Is.EqualTo(System.TypeCode.Boolean));
        }

        [Test]
        public void BoolFalseLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("false", p => p.literalConstant())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(false));
        }

        [Test]
        public void RealLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("3.14", p => p.literalConstant())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(3.14).Within(0.0001));
        }

        [Test]
        public void NullLiteralTest()
        {
            NullLitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("null", p => p.literalConstant())
                .As<NullLitExprNode>();
            Assert.That(node.Value, Is.Null);
        }

        // --- simpleIdentifier ---

        [Test]
        public void SimpleIdentifierTest()
        {
            IdNode node = new KotlinASTBuilder()
                .BuildFromSource("x", p => p.simpleIdentifier())
                .As<IdNode>();
            Assert.That(node.Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void MultiWordIdentifierTest()
        {
            IdNode node = new KotlinASTBuilder()
                .BuildFromSource("myVariable", p => p.simpleIdentifier())
                .As<IdNode>();
            Assert.That(node.Identifier, Is.EqualTo("myVariable"));
        }

        // --- parenthesizedExpression ---
        // Requires: VisitLiteralConstant, VisitAtomicExpression, VisitPostfixUnaryExpression,
        //           VisitPrefixUnaryExpression, VisitTypeRHS, VisitMultiplicativeExpression,
        //           VisitAdditiveExpression (and pass-throughs up to disjunction)

        [Test]
        public void ParenthesizedLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("(42)", p => p.parenthesizedExpression())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        // --- atomicExpression ---
        // Requires: VisitLiteralConstant and VisitSimpleIdentifier

        [Test]
        public void AtomicLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.atomicExpression())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void AtomicIdentifierTest()
        {
            IdNode node = new KotlinASTBuilder()
                .BuildFromSource("x", p => p.atomicExpression())
                .As<IdNode>();
            Assert.That(node.Identifier, Is.EqualTo("x"));
        }

        // --- postfixUnaryExpression ---
        // Requires: VisitAtomicExpression, VisitLiteralConstant

        [Test]
        public void PostfixNoOpPassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.postfixUnaryExpression())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        // --- prefixUnaryExpression ---
        // Requires: VisitPostfixUnaryExpression and chain below

        [Test]
        public void PrefixNonePassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.prefixUnaryExpression())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void NegationTest()
        {
            // -42  →  UnaryExprNode("-", LitExprNode(42))
            UnaryExprNode node = new KotlinASTBuilder()
                .BuildFromSource("-42", p => p.prefixUnaryExpression())
                .As<UnaryExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("-"));
            Assert.That(node.Operand.As<LitExprNode>().Value, Is.EqualTo(42));
        }

        [Test]
        public void LogicalNotTest()
        {
            // !true  →  UnaryExprNode("!", LitExprNode(true))
            UnaryExprNode node = new KotlinASTBuilder()
                .BuildFromSource("!true", p => p.prefixUnaryExpression())
                .As<UnaryExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("!"));
            Assert.That(node.Operand.As<LitExprNode>().Value, Is.EqualTo(true));
        }

        // --- multiplicativeExpression ---
        // Requires: full chain from typeRHS down to literalConstant/simpleIdentifier

        [Test]
        public void MultiplicativePassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.multiplicativeExpression())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void MultiplyTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("3 * 4", p => p.multiplicativeExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("*"));
            Assert.That(node.LeftOperand.As<LitExprNode>().Value, Is.EqualTo(3));
            Assert.That(node.RightOperand.As<LitExprNode>().Value, Is.EqualTo(4));
        }

        [Test]
        public void DivideTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("10 / 2", p => p.multiplicativeExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("/"));
        }

        [Test]
        public void ModuloTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("7 % 3", p => p.multiplicativeExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("%"));
        }

        // --- additiveExpression ---
        // Requires: full chain from multiplicativeExpression down

        [Test]
        public void AdditivePassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.additiveExpression())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void AddTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("1 + 2", p => p.additiveExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("+"));
            Assert.That(node.LeftOperand.As<LitExprNode>().Value, Is.EqualTo(1));
            Assert.That(node.RightOperand.As<LitExprNode>().Value, Is.EqualTo(2));
        }

        [Test]
        public void SubtractTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("5 - 3", p => p.additiveExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("-"));
        }

        // --- equalityComparison ---
        // Requires: full chain from comparison → namedInfix → elvisExpression → infixFunctionCall
        //           → rangeExpression → additiveExpression → ... down to literalConstant/simpleIdentifier
        // All intermediate single-child pass-throughs must be implemented.

        [Test]
        public void EqualityPassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.equalityComparison())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void EqualTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x == y", p => p.equalityComparison())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("=="));
            Assert.That(node.LeftOperand.As<IdNode>().Identifier, Is.EqualTo("x"));
            Assert.That(node.RightOperand.As<IdNode>().Identifier, Is.EqualTo("y"));
        }

        [Test]
        public void NotEqualTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x != y", p => p.equalityComparison())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("!="));
        }

        // --- comparison ---
        // Same chain requirement as equalityComparison

        [Test]
        public void LessThanTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x < y", p => p.comparison())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("<"));
        }

        [Test]
        public void GreaterThanTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x > y", p => p.comparison())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo(">"));
        }

        [Test]
        public void LessOrEqualTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x <= y", p => p.comparison())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("<="));
        }

        [Test]
        public void GreaterOrEqualTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x >= y", p => p.comparison())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo(">="));
        }

        // --- conjunction ---

        [Test]
        public void ConjunctionPassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.conjunction())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void AndTest()
        {
            LogicExprNode node = new KotlinASTBuilder()
                .BuildFromSource("true && false", p => p.conjunction())
                .As<LogicExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("&&"));
            Assert.That(node.LeftOperand.As<LitExprNode>().Value, Is.EqualTo(true));
            Assert.That(node.RightOperand.As<LitExprNode>().Value, Is.EqualTo(false));
        }

        // --- disjunction ---

        [Test]
        public void DisjunctionPassThroughTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("42", p => p.disjunction())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(42));
        }

        [Test]
        public void OrTest()
        {
            LogicExprNode node = new KotlinASTBuilder()
                .BuildFromSource("true || false", p => p.disjunction())
                .As<LogicExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("||"));
        }

        // --- rangeExpression ---

        [Test]
        public void RangeExpressionTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("1..10", p => p.rangeExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo(".."));
            Assert.That(node.LeftOperand.As<LitExprNode>().Value, Is.EqualTo(1));
            Assert.That(node.RightOperand.As<LitExprNode>().Value, Is.EqualTo(10));
        }

        // --- elvisExpression ---

        [Test]
        public void ElvisExpressionTest()
        {
            ArithmExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x ?: y", p => p.elvisExpression())
                .As<ArithmExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("?:"));
            Assert.That(node.LeftOperand.As<IdNode>().Identifier, Is.EqualTo("x"));
            Assert.That(node.RightOperand.As<IdNode>().Identifier, Is.EqualTo("y"));
        }

        // --- string literal ---

        [Test]
        public void StringLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("\"hello world\"", p => p.stringLiteral())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo("hello world"));
            Assert.That(node.TypeCode, Is.EqualTo(System.TypeCode.String));
        }

        [Test]
        public void EmptyStringLiteralTest()
        {
            LitExprNode node = new KotlinASTBuilder()
                .BuildFromSource("\"\"", p => p.stringLiteral())
                .As<LitExprNode>();
            Assert.That(node.Value, Is.EqualTo(""));
        }

        // --- is / !is operators ---

        [Test]
        public void IsOperatorTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x is String", p => p.namedInfix())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("is"));
        }

        [Test]
        public void NotIsOperatorTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x !is String", p => p.namedInfix())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("!is"));
        }

        // --- in / !in operators ---

        [Test]
        public void InOperatorTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x in items", p => p.namedInfix())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("in"));
        }

        [Test]
        public void NotInOperatorTest()
        {
            RelExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x !in items", p => p.namedInfix())
                .As<RelExprNode>();
            Assert.That(node.Operator.Symbol, Is.EqualTo("!in"));
        }

        // --- as / as? type casts ---

        [Test]
        public void AsTypeCastTest()
        {
            FuncCallExprNode node = new KotlinASTBuilder()
                .BuildFromSource("x as String", p => p.typeRHS())
                .As<FuncCallExprNode>();
            Assert.That(node.Identifier, Is.EqualTo("__linvast_cast"));
            Assert.That(node.Arguments, Is.Not.Null);
            Assert.That(node.Arguments!.Expressions, Has.Exactly(2).Items);
        }

        // --- callable reference ---

        [Test]
        public void CallableReferenceTest()
        {
            IdNode node = new KotlinASTBuilder()
                .BuildFromSource("::foo", p => p.callableReference())
                .As<IdNode>();
            Assert.That(node.Identifier, Is.EqualTo("__linvast_callable_ref:foo"));
        }

        // --- postfix operations ---

        [Test]
        public void FunctionCallTest()
        {
            FuncCallExprNode node = new KotlinASTBuilder()
                .BuildFromSource("foo()", p => p.postfixUnaryExpression())
                .As<FuncCallExprNode>();
            Assert.That(node.Identifier, Is.EqualTo("foo"));
        }

        [Test]
        public void FunctionCallWithArgumentTest()
        {
            FuncCallExprNode node = new KotlinASTBuilder()
                .BuildFromSource("foo(42)", p => p.postfixUnaryExpression())
                .As<FuncCallExprNode>();
            Assert.That(node.Identifier, Is.EqualTo("foo"));
            Assert.That(node.Arguments, Is.Not.Null);
            Assert.That(node.Arguments!.Expressions, Has.Exactly(1).Items);
        }

        [Test]
        public void MemberAccessTest()
        {
            IdNode node = new KotlinASTBuilder()
                .BuildFromSource("foo.bar", p => p.postfixUnaryExpression())
                .As<IdNode>();
            Assert.That(node.Identifier, Is.EqualTo("foo.bar"));
        }

        [Test]
        public void MethodCallOnObjectTest()
        {
            FuncCallExprNode node = new KotlinASTBuilder()
                .BuildFromSource("foo.bar()", p => p.postfixUnaryExpression())
                .As<FuncCallExprNode>();
            Assert.That(node.Identifier, Is.EqualTo("foo.bar"));
        }

        [Test]
        public void ArrayAccessTest()
        {
            ArrAccessExprNode node = new KotlinASTBuilder()
                .BuildFromSource("arr[0]", p => p.postfixUnaryExpression())
                .As<ArrAccessExprNode>();
            Assert.That(node.Array.As<IdNode>().Identifier, Is.EqualTo("arr"));
            Assert.That(node.IndexExpression.As<LitExprNode>().Value, Is.EqualTo(0));
        }
    }
}
