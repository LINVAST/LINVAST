using System;
using System.Linq;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Visitors
{
    internal sealed class BaseASTVisitorTests
    {
        [Test]
        public void TraversalHasNoCumulativeLimit()
        {
            var visitor = new CountingVisitor();
            ASTNode tree = new SourceNode(Enumerable.Range(0, 5000).Select(i => new IdNode(1, "v" + i)));
            Assert.That(visitor.Visit(tree), Is.EqualTo(5001));
            Assert.That(visitor.Visit(tree), Is.EqualTo(5001));
        }

        [Test]
        public void VisitorCanBeReusedForManySmallTrees()
        {
            var visitor = new CountingVisitor();
            for (int i = 0; i < 2500; i++)
                Assert.That(visitor.Visit((ASTNode)new ExprStatNode(1, new IdNode(1, "x"))), Is.EqualTo(2));
        }

        [Test]
        public void UnknownAndUnlistedNodesFallBackToTheirChildren()
        {
            ASTNode tree = new CustomNode(new TypeNameNode(1, "List", new TypeNameNode(1, "int")));
            Assert.That(new CountingVisitor().Visit(tree), Is.EqualTo(5));
        }

        [Test]
        public void MostSpecificOverridesAreDispatchedThroughASTNode()
        {
            var visitor = new DispatchVisitor();
            Assert.That(visitor.Visit((ASTNode)new IncExprNode(1, new IdNode(1, "x"))), Is.EqualTo("increment"));
            Assert.That(visitor.Visit((ASTNode)new NullLitExprNode(1)), Is.EqualTo("null"));
            Assert.That(visitor.Visit((ASTNode)new ThrowStatNode(1, new IdNode(1, "error"))), Is.EqualTo("throw"));
            Assert.That(visitor.Visit((ASTNode)new ConsExprNode(1, new IdNode(1, "Widget"))), Is.EqualTo("call"));
        }

        [Test]
        public void ShortCircuitAndAggregationStillApply()
        {
            ASTNode tree = new SourceNode(new IdNode(1, "first"), new IdNode(1, "second"));
            Assert.That(new FirstIdentifierVisitor().Visit(tree), Is.EqualTo("first"));
        }

        [Test]
        public void FailedVisitDoesNotAffectSubsequentTraversal()
        {
            var visitor = new ThrowOnceVisitor();
            ASTNode tree = new SourceNode(new IdNode(1, "x"));
            Assert.Throws<InvalidOperationException>(() => visitor.Visit(tree));
            Assert.That(visitor.Visit(tree), Is.EqualTo(2));
        }

        private class CountingVisitor : BaseASTVisitor<int>
        {
            public override int VisitChildren(ASTNode node) => 1 + base.VisitChildren(node);
            protected override int AggregateResult(int aggregate, int nextResult) => aggregate + nextResult;
        }

        private sealed class CustomNode : ASTNode
        {
            public CustomNode(params ASTNode[] children) : base(1, children) { }
        }

        private sealed class DispatchVisitor : BaseASTVisitor<string>
        {
            public override string Visit(IncExprNode node) => "increment";
            public override string Visit(AssignExprNode node) => "assignment";
            public override string Visit(NullLitExprNode node) => "null";
            public override string Visit(LitExprNode node) => "literal";
            public override string Visit(ThrowStatNode node) => "throw";
            public override string Visit(ExprStatNode node) => "statement";
            public override string Visit(FuncCallExprNode node) => "call";
        }

        private sealed class FirstIdentifierVisitor : BaseASTVisitor<string?>
        {
            public override string Visit(IdNode node) => node.Identifier;
            protected override bool ShouldVisitNextChild(ASTNode node, string? currentResult) => currentResult is null;
        }

        private sealed class ThrowOnceVisitor : CountingVisitor
        {
            private bool throwNext = true;
            public override int Visit(IdNode node)
            {
                if (this.throwNext) {
                    this.throwNext = false;
                    throw new InvalidOperationException("Test visitor failure.");
                }
                return this.VisitChildren(node);
            }
        }
    }
}
