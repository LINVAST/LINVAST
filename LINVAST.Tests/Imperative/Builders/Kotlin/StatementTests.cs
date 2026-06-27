using System;
using System.Linq;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class StatementTests : ASTBuilderTestBase
    {
        protected override ASTNode GenerateAST(string src)
            => new KotlinASTBuilder().BuildFromSource(src, p => p.ifExpression());

        // --- jumpExpression ---

        [Test]
        public void ReturnVoidTest()
        {
            JumpStatNode node = new KotlinASTBuilder()
                .BuildFromSource("return", p => p.jumpExpression())
                .As<JumpStatNode>();
            Assert.That(node.ReturnExpr, Is.Null);
        }

        [Test]
        public void ReturnValueTest()
        {
            JumpStatNode node = new KotlinASTBuilder()
                .BuildFromSource("return 42", p => p.jumpExpression())
                .As<JumpStatNode>();
            Assert.That(node.ReturnExpr, Is.Not.Null);
            Assert.That(node.ReturnExpr!.As<LitExprNode>().Value, Is.EqualTo(42));
        }

        [Test]
        public void ReturnExpressionTest()
        {
            JumpStatNode node = new KotlinASTBuilder()
                .BuildFromSource("return x", p => p.jumpExpression())
                .As<JumpStatNode>();
            Assert.That(node.ReturnExpr, Is.Not.Null);
            Assert.That(node.ReturnExpr!.As<IdNode>().Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void BreakThrowsTest()
        {
            Assert.That(
                () => new KotlinASTBuilder().BuildFromSource("break", p => p.jumpExpression()),
                Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void ContinueThrowsTest()
        {
            Assert.That(
                () => new KotlinASTBuilder().BuildFromSource("continue", p => p.jumpExpression()),
                Throws.InstanceOf<NotImplementedException>());
        }

        // --- ifExpression ---

        [Test]
        public void SimpleIfTest()
        {
            IfStatNode node = this.GenerateAST("if (true) { }").As<IfStatNode>();
            Assert.That(node.Condition.As<LitExprNode>().Value, Is.EqualTo(true));
            Assert.That(node.ElseStat, Is.Null);
        }

        [Test]
        public void IfWithBodyTest()
        {
            IfStatNode node = this.GenerateAST("if (true) { return 1 }").As<IfStatNode>();
            Assert.That(node.Condition.As<LitExprNode>().Value, Is.EqualTo(true));
            Assert.That(node.ThenStat, Is.InstanceOf<BlockStatNode>());
            Assert.That(node.ThenStat.Children, Has.Exactly(1).Items);
        }

        [Test]
        public void IfElseTest()
        {
            IfStatNode node = this.GenerateAST("if (true) { } else { }").As<IfStatNode>();
            Assert.That(node.Condition.As<LitExprNode>().Value, Is.EqualTo(true));
            Assert.That(node.ElseStat, Is.Not.Null);
        }

        [Test]
        public void IfElseWithBodyTest()
        {
            IfStatNode node = this.GenerateAST("if (x == y) { return 1 } else { return 2 }").As<IfStatNode>();
            Assert.That(node.Condition, Is.InstanceOf<RelExprNode>());
            Assert.That(node.ThenStat.Children, Has.Exactly(1).Items);
            Assert.That(node.ElseStat, Is.Not.Null);
            Assert.That(node.ElseStat!.Children, Has.Exactly(1).Items);
        }

        // --- whileExpression ---

        [Test]
        public void SimpleWhileTest()
        {
            WhileStatNode node = new KotlinASTBuilder()
                .BuildFromSource("while (true) { }", p => p.whileExpression())
                .As<WhileStatNode>();
            Assert.That(node.Condition.As<LitExprNode>().Value, Is.EqualTo(true));
            Assert.That(node.Statement, Is.InstanceOf<BlockStatNode>());
        }

        [Test]
        public void WhileWithBodyTest()
        {
            WhileStatNode node = new KotlinASTBuilder()
                .BuildFromSource("while (x > 0) { return x }", p => p.whileExpression())
                .As<WhileStatNode>();
            Assert.That(node.Condition, Is.InstanceOf<RelExprNode>());
            Assert.That(node.Statement.Children, Has.Exactly(1).Items);
        }

        // --- expression (assignment) ---

        [Test]
        public void SimpleAssignmentTest()
        {
            ExprStatNode node = new KotlinASTBuilder()
                .BuildFromSource("x = 42", p => p.expression())
                .As<ExprStatNode>();
            AssignExprNode assign = node.Expression.As<AssignExprNode>();
            Assert.That(assign.LeftOperand.As<IdNode>().Identifier, Is.EqualTo("x"));
            Assert.That(assign.RightOperand.As<LitExprNode>().Value, Is.EqualTo(42));
            Assert.That(assign.Operator.Symbol, Is.EqualTo("="));
        }

        [Test]
        public void CompoundAssignmentTest()
        {
            ExprStatNode node = new KotlinASTBuilder()
                .BuildFromSource("x += 1", p => p.expression())
                .As<ExprStatNode>();
            AssignExprNode assign = node.Expression.As<AssignExprNode>();
            Assert.That(assign.Operator.Symbol, Is.EqualTo("+="));
        }

        // --- throws ---

        [Test]
        public void DoWhileThrowsTest()
        {
            Assert.That(
                () => new KotlinASTBuilder().BuildFromSource("do { } while (true)", p => p.doWhileExpression()),
                Throws.InstanceOf<NotImplementedException>());
        }

        // --- forExpression ---

        [Test]
        public void SimpleForTest()
        {
            ForeachStatNode node = new KotlinASTBuilder()
                .BuildFromSource("for (x in items) { }", p => p.forExpression())
                .As<ForeachStatNode>();
            Assert.That(node.Iterator.Identifier, Is.EqualTo("x"));
            Assert.That(node.Iterable.As<IdNode>().Identifier, Is.EqualTo("items"));
            Assert.That(node.Statement, Is.InstanceOf<BlockStatNode>());
        }

        [Test]
        public void ForWithExplicitTypeTest()
        {
            ForeachStatNode node = new KotlinASTBuilder()
                .BuildFromSource("for (x: Int in items) { }", p => p.forExpression())
                .As<ForeachStatNode>();
            Assert.That(node.Iterator.Identifier, Is.EqualTo("x"));
            Assert.That(node.IteratorType.TypeName, Is.EqualTo("Int"));
            Assert.That(node.Iterable.As<IdNode>().Identifier, Is.EqualTo("items"));
        }

        [Test]
        public void ForWithBodyTest()
        {
            ForeachStatNode node = new KotlinASTBuilder()
                .BuildFromSource("for (x in items) { return x }", p => p.forExpression())
                .As<ForeachStatNode>();
            Assert.That(node.Statement.As<BlockStatNode>().Children, Has.Exactly(1).Items);
            Assert.That(node.Statement.As<BlockStatNode>().Children.Single(), Is.InstanceOf<JumpStatNode>());
        }

        [Test]
        public void ForWithFunctionCallIterableThrowsTest()
        {
            Assert.That(
                () => new KotlinASTBuilder().BuildFromSource("for (x in listOf(1, 2, 3)) { }", p => p.forExpression()),
                Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void ForDestructuringTwoVarsTest()
        {
            ForeachStatNode node = new KotlinASTBuilder()
                .BuildFromSource("for ((a, b) in pairs) { }", p => p.forExpression())
                .As<ForeachStatNode>();
            Assert.That(node.IteratorDeclaration.DeclaratorList.Declarators, Has.Exactly(2).Items);
            Assert.That(node.IteratorDeclaration.DeclaratorList.Declarators.ElementAt(0).Identifier, Is.EqualTo("a"));
            Assert.That(node.IteratorDeclaration.DeclaratorList.Declarators.ElementAt(1).Identifier, Is.EqualTo("b"));
            Assert.That(node.Iterable.As<IdNode>().Identifier, Is.EqualTo("pairs"));
        }

        [Test]
        public void ForDestructuringThreeVarsTest()
        {
            ForeachStatNode node = new KotlinASTBuilder()
                .BuildFromSource("for ((a, b, c) in triples) { }", p => p.forExpression())
                .As<ForeachStatNode>();
            Assert.That(node.IteratorDeclaration.DeclaratorList.Declarators, Has.Exactly(3).Items);
            Assert.That(node.IteratorDeclaration.DeclaratorList.Declarators.ElementAt(2).Identifier, Is.EqualTo("c"));
        }
    }
}
