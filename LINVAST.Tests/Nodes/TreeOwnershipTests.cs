using System;
using System.Collections.Generic;
using System.Linq;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using LINVAST.Nodes;
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class TreeOwnershipTests
    {
        [Test]
        public void ConstructorCopiesAndProtectsChildArray()
        {
            var original = new IdNode(1, "x");
            ASTNode[] children = { original };
            var node = new SourceNode(children);
            children[0] = new IdNode(1, "y");
            Assert.That(node.Children[0], Is.SameAs(original));
            Assert.That(original.Parent, Is.SameAs(node));
            Assert.That(node.Children, Is.Not.InstanceOf<ASTNode[]>());
            Assert.Throws<NotSupportedException>(() => ((IList<ASTNode>)node.Children)[0] = children[0]);
        }

        [Test]
        public void TagValuesParticipateInTraversalCopyAndSubstitution()
        {
            var value = new LitExprNode(1, 42);
            var tag = new TagNode(1, "A", new[] { new TagFieldNode(1, "value", value) });
            Assert.That(value.Parent, Is.SameAs(tag.Fields.Single()));
            var visitor = new LiteralCollector();
            visitor.Visit(tag);
            Assert.That(visitor.Values, Is.EqualTo(new[] { 42 }));

            var copy = tag.Copy().As<TagNode>();
            var copiedValue = copy.Children[1].As<TagFieldNode>().Value;
            Assert.That(copiedValue, Is.Not.SameAs(value));
            Assert.That(copiedValue.Parent, Is.SameAs(copy.Children[1]));
            LoopInvariantTests.AssertOwnedTree(copy);

            var changed = tag.Substitute(value, new LitExprNode(1, 99)).As<TagNode>();
            Assert.That(changed.Children[1].As<TagFieldNode>().Value.Value, Is.EqualTo(99));
            Assert.That(value.Value, Is.EqualTo(42));
            LoopInvariantTests.AssertOwnedTree(changed);
        }

        [Test]
        public void TagValueSerializesOnceAsAChild()
        {
            var field = new TagFieldNode(1, "value", new LitExprNode(1, 42));
            JObject json = JObject.Parse(field.ToJson());
            Assert.That(json["Value"], Is.Null);
            Assert.That(json["Children"]!.Count(), Is.EqualTo(1));
            Assert.That(json["Children"]![0]!["Value"]!.Value<int>(), Is.EqualTo(42));
        }

        private sealed class LiteralCollector : BaseASTVisitor<int>
        {
            public List<object?> Values { get; } = new();
            public override int Visit(LitExprNode node) { this.Values.Add(node.Value); return 1; }
        }
    }
}
