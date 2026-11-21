using System;
using System.Collections.Generic;
using System.Globalization;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class StructuralHashTests
    {
        [Test]
        public void PointerLevelsParticipateInEquality()
        {
            var plain = new VarDeclNode(1, new IdNode(1, "p"));
            var pointer = new VarDeclNode(1, new IdNode(1, "p")) { PointerLevel = 1 };
            Assert.That(plain, Is.Not.EqualTo(pointer));
            Assert.That(pointer, Is.Not.EqualTo(plain));
            Assert.That(new HashSet<ASTNode> { plain, pointer }.Count, Is.EqualTo(2));
            AssertEqualAndHash(pointer, pointer.Copy());
        }

        [Test]
        public void ImportsAndPackagesIncludeNamesAndAliases()
        {
            Assert.That(new ImportNode(1, "a"), Is.Not.EqualTo(new ImportNode(1, "b")));
            Assert.That(new ImportNode(1, "a", "x"), Is.Not.EqualTo(new ImportNode(1, "a", "y")));
            Assert.That(new ImportNode(1, "a"), Is.Not.EqualTo(new ImportNode(1, "a", "x")));
            Assert.That(new PackageNode(1, "a"), Is.Not.EqualTo(new PackageNode(1, "b")));
            AssertEqualAndHash(new ImportNode(1, "a", "x"), new ImportNode(9, "a", "x"));
            AssertEqualAndHash(new PackageNode(1, "a"), new PackageNode(9, "a"));
        }

        [Test]
        public void TryClauseRolesParticipateInEquality()
        {
            var withElse = new TryStatNode(1, new BlockStatNode(1), Array.Empty<CatchClauseNode>(), new BlockStatNode(1), null);
            var withFinally = new TryStatNode(1, new BlockStatNode(1), Array.Empty<CatchClauseNode>(), null, new BlockStatNode(1));
            Assert.That(withElse, Is.Not.EqualTo(withFinally));
            Assert.That(new HashSet<ASTNode> { withElse, withFinally }.Count, Is.EqualTo(2));
            AssertEqualAndHash(withElse, withElse.Copy());
            AssertEqualAndHash(withFinally, withFinally.Copy());
        }

        [Test]
        public void EqualLiteralValuesNeedNotHaveIdenticalText()
        {
            AssertEqualAndHash(new LitExprNode(1, 1.0m), new LitExprNode(2, 1.00m));
            AssertEqualAndHash(new LitExprNode(1, 0.0), new LitExprNode(2, -0.0));
            AssertEqualAndHash(new LitExprNode(1, 1L, "l"), new LitExprNode(2, 1L, "ll"));
        }

        [Test]
        public void HashCollectionsSurviveCultureChanges()
        {
            CultureInfo previous = CultureInfo.CurrentCulture;
            try {
                CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("en-US");
                var node = new LitExprNode(1, 1.5);
                var set = new HashSet<ASTNode> { node };
                int hash = node.GetHashCode();
                CultureInfo.CurrentCulture = CultureInfo.GetCultureInfo("de-DE");
                Assert.That(node.GetHashCode(), Is.EqualTo(hash));
                Assert.That(set.Contains(new LitExprNode(8, 1.5)), Is.True);
            } finally {
                CultureInfo.CurrentCulture = previous;
            }
        }

        [Test]
        public void HashingDoesNotRenderNodes()
            => AssertEqualAndHash(new UnrenderableNode(1, new IdNode(1, "x")), new UnrenderableNode(4, new IdNode(4, "x")));

        [Test]
        public void MetadataNodesCopyWithConsistentHashes()
        {
            ASTNode[] nodes = {
                new IdNode(1, "x"), new LitExprNode(1, 42), new NullLitExprNode(1), new EllipsisLitExprNode(1),
                ArithmOpNode.FromSymbol(1, "+"), new DeclSpecsNode(1, "const", "int"),
                new FuncParamsNode(1) { IsVariadic = true }, new JumpStatNode(1, JumpStatType.Break),
                new LabeledStatNode(1, "done", new EmptyStatNode(1)),
                new YieldExprNode(1, new IdNode(1, "xs"), isDelegating: true),
                new TagFieldNode(1, "value", new LitExprNode(1, 42))
            };
            foreach (ASTNode node in nodes)
                AssertEqualAndHash(node, node.Copy());
            AssertEqualAndHash(new SourceNode("a.c", new IdNode(1, "x")), new SourceNode("b.c", new IdNode(2, "x")));
        }

        private static void AssertEqualAndHash(ASTNode left, ASTNode right)
        {
            Assert.That(left.Equals(right), Is.True);
            Assert.That(right.Equals(left), Is.True);
            Assert.That(left.GetHashCode(), Is.EqualTo(right.GetHashCode()));
            Assert.That(new HashSet<ASTNode> { left }.Contains(right), Is.True);
        }

        private sealed class UnrenderableNode : ASTNode
        {
            public UnrenderableNode(int line, params ASTNode[] children) : base(line, children) { }
            public override string GetText() => throw new InvalidOperationException("Rendering must not be used for hashing.");
        }
    }
}
