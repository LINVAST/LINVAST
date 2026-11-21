using System;
using System.Collections.Generic;
using System.Linq;
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
using Newtonsoft.Json.Linq;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class LoopInvariantTests
    {
        private static ForStatNode CreateLoop(string? initializer = "init", string? increment = "step", string condition = "condition", string body = "body", int line = 1)
            => new ForStatNode(line, initializer is null ? null : new IdNode(line, initializer),
                new IdNode(line, condition), increment is null ? null : new IdNode(line, increment),
                new ExprStatNode(line, new IdNode(line, body)));

        [TestCase(false, false)]
        [TestCase(false, true)]
        [TestCase(true, false)]
        [TestCase(true, true)]
        public void OptionalComponentsAreChildrenWithParents(bool initializer, bool increment)
        {
            var loop = CreateLoop(initializer ? "init" : null, increment ? "step" : null);
            Assert.That(loop.HasInitializer, Is.EqualTo(initializer));
            Assert.That(loop.HasIncrement, Is.EqualTo(increment));
            Assert.That(loop.Children.Count, Is.EqualTo(2 + (initializer ? 1 : 0) + (increment ? 1 : 0)));
            Assert.That(loop.Children[0], Is.SameAs(loop.Condition));
            Assert.That(loop.Children[1], Is.SameAs(loop.Statement));
            AssertOwnedTree(loop);

            var visitor = new IdentifierVisitor();
            visitor.Visit((ASTNode)loop);
            var expected = new List<string> { "condition", "body" };
            if (initializer) expected.Add("init");
            if (increment) expected.Add("step");
            Assert.That(visitor.Identifiers, Is.EqualTo(expected));
            AssertCopy(loop);
        }

        [TestCase("init", "changed", "step", "condition", "body")]
        [TestCase("step", "init", "changed", "condition", "body")]
        [TestCase("condition", "init", "step", "changed", "body")]
        [TestCase("body", "init", "step", "condition", "changed")]
        public void EveryComponentParticipatesInEqualityAndSubstitution(string target, string initializer, string increment, string condition, string body)
        {
            var original = CreateLoop();
            var expected = CreateLoop(initializer, increment, condition, body);
            Assert.That(original.Equals(expected), Is.False);
            Assert.That(expected.Equals(original), Is.False);
            var rewritten = original.Substitute(new IdNode(1, target), new IdNode(1, "changed"));
            Assert.That(rewritten, Is.EqualTo(expected));
            Assert.That(original, Is.EqualTo(CreateLoop()));
            AssertOwnedTree(rewritten);
        }

        [Test]
        public void InitializerAndIncrementHaveDistinctRoles()
        {
            Assert.That(CreateLoop("x", null).Equals(CreateLoop(null, "x")), Is.False);
            Assert.That(CreateLoop(null, "x").Equals(CreateLoop("x", null)), Is.False);
        }

        [Test]
        public void EqualityIgnoresLineNumbersAndHasConsistentHashes()
        {
            var first = CreateLoop(line: 1);
            var second = CreateLoop(line: 8);
            Assert.That(first, Is.EqualTo(second));
            Assert.That(first.GetHashCode(), Is.EqualTo(second.GetHashCode()));
            Assert.That(new HashSet<ASTNode> { first }.Contains(second), Is.True);
        }

        [Test]
        public void DeclarationInitializerIsCopiedAndSubstituted()
        {
            var declaration = new DeclListNode(1, new VarDeclNode(1, new IdNode(1, "iterator"), new IdNode(1, "seed")));
            var loop = new ForStatNode(1, declaration, null, null, new EmptyStatNode(1));
            Assert.That(loop.ForDeclaration, Is.SameAs(declaration));
            Assert.That(loop.InitExpr, Is.Null);
            Assert.That(loop.Condition.As<LitExprNode>().Value, Is.EqualTo(true));
            AssertCopy(loop);

            var rewritten = loop.Substitute<ForStatNode>(new IdNode(1, "seed"), new LitExprNode(1, 10));
            Assert.That(rewritten.ForDeclaration!.As<DeclListNode>().Declarators.Single().As<VarDeclNode>().Initializer,
                Is.EqualTo(new LitExprNode(1, 10)));
            Assert.That(rewritten.Equals(loop), Is.False);
            AssertOwnedTree(rewritten);
        }

        [Test]
        public void LoopBodyDistinguishesBreakFromContinue()
        {
            var first = new WhileStatNode(1, new LitExprNode(1, true), new JumpStatNode(1, JumpStatType.Break));
            var second = new WhileStatNode(1, new LitExprNode(1, true), new JumpStatNode(1, JumpStatType.Continue));
            Assert.That(first.Equals(second), Is.False);
            Assert.That(first.Substitute(first.Statement, second.Statement), Is.EqualTo(second));
        }

        [TestCase("C")]
        [TestCase("Go")]
        [TestCase("Java")]
        [TestCase("JavaForeach")]
        [TestCase("Kotlin")]
        [TestCase("Lua")]
        [TestCase("Python")]
        [TestCase("While")]
        public void ParsedLoopsSupportTraversalCopyAndSubstitution(string language)
        {
            ASTNode loop = language switch {
                "C" => new CASTBuilder().BuildFromSource("for (int i = seed; i < limit; i++) { consume(i); }", p => p.statement()),
                "Go" => new GoASTBuilder().BuildFromSource("for i := seed; i < limit; i++ { consume(i) }", p => p.statement()),
                "Java" => new JavaASTBuilder().BuildFromSource("for (int i = seed; i < limit; i++) { consume(i); }", p => p.statement()),
                "JavaForeach" => new JavaASTBuilder().BuildFromSource("for (int i : items) { consume(i); }", p => p.statement()),
                "Kotlin" => new KotlinASTBuilder().BuildFromSource("for (i in items) { consume(i) }", p => p.forExpression()),
                "Lua" => new LuaASTBuilder().BuildFromSource("for i = seed, limit do consume(i) end", p => p.stat()),
                "Python" => new PythonASTBuilder().BuildFromSource("for i in items:\n    consume(i)\n", p => p.for_stmt()),
                _ => new CASTBuilder().BuildFromSource("while (i < limit) { consume(i); }", p => p.statement()),
            };
            AssertCopy(loop);
            var visitor = new IdentifierVisitor();
            visitor.Visit(loop);
            Assert.That(visitor.Identifiers, Does.Contain("i"));
            if (loop is ForStatNode counted) {
                ASTNode? initializer = counted.ForDeclaration ?? (ASTNode?)counted.InitExpr;
                if (initializer is not null)
                    Assert.That(initializer.Parent, Is.SameAs(loop));
                if (counted.IncrExpr is not null)
                    Assert.That(counted.IncrExpr.Parent, Is.SameAs(loop));
            }

            var rewritten = loop.Substitute(new IdNode(1, "i"), new IdNode(1, "renamed"));
            var rewrittenVisitor = new IdentifierVisitor();
            rewrittenVisitor.Visit(rewritten);
            Assert.That(rewrittenVisitor.Identifiers, Does.Not.Contain("i"));
            Assert.That(rewrittenVisitor.Identifiers.Count(id => id == "renamed"), Is.EqualTo(visitor.Identifiers.Count(id => id == "i")));
            Assert.That(rewritten.Equals(loop), Is.False);
            AssertOwnedTree(rewritten);
        }

        [Test]
        public void NestedLoopsCopyEveryComponentAndDetachTheRoot()
        {
            var inner = CreateLoop();
            var outer = new ForeachStatNode(1, new TypeNameNode(1, "int"), new IdNode(1, "item"), new IdNode(1, "items"), inner);
            var source = new SourceNode(new WhileStatNode(1, new IdNode(1, "ready"), outer));
            AssertCopy(source);
            var detached = inner.Copy();
            Assert.That(detached.Parent, Is.Null);
            Assert.That(inner.Parent, Is.SameAs(outer));
            AssertOwnedTree(detached);
        }

        [Test]
        public void JsonStoresEachLoopComponentOnceAndRecordsItsRole()
        {
            var json = JObject.Parse(CreateLoop(null, "step").ToJson());
            Assert.That((bool)json["HasInitializer"]!, Is.False);
            Assert.That((bool)json["HasIncrement"]!, Is.True);
            Assert.That(json["InitExpr"], Is.Null);
            Assert.That(json["IncrExpr"], Is.Null);
            Assert.That(json["ForDeclaration"], Is.Null);
            Assert.That(json["Children"]!.Count(), Is.EqualTo(3));
            Assert.That(json.SelectTokens("$..Identifier").Select(token => (string)token!), Is.EqualTo(new[] { "condition", "body", "step" }));
        }

        private static void AssertCopy(ASTNode original)
        {
            var originals = Descendants(original).ToArray();
            var parents = originals.Select(node => node.Parent).ToArray();
            var copy = original.Copy();
            Assert.That(copy, Is.EqualTo(original));
            Assert.That(copy.GetText(), Is.EqualTo(original.GetText()));
            Assert.That(copy.GetHashCode(), Is.EqualTo(original.GetHashCode()));
            Assert.That(copy.Parent, Is.Null);
            Assert.That(Descendants(copy).Count(), Is.EqualTo(originals.Length));
            var originalSet = new HashSet<ASTNode>(originals, ReferenceEqualityComparer.Instance);
            Assert.That(Descendants(copy).Any(originalSet.Contains), Is.False);
            AssertOwnedTree(copy);
            for (int i = 0; i < originals.Length; i++)
                Assert.That(originals[i].Parent, Is.SameAs(parents[i]));
        }

        internal static void AssertOwnedTree(ASTNode root)
        {
            var seen = new HashSet<ASTNode>(ReferenceEqualityComparer.Instance);
            foreach (var parent in Descendants(root)) {
                Assert.That(seen.Add(parent), Is.True, "A copied tree must not share node instances.");
                foreach (var child in parent.Children)
                    Assert.That(child.Parent, Is.SameAs(parent));
            }
        }

        internal static IEnumerable<ASTNode> Descendants(ASTNode root)
        {
            yield return root;
            foreach (var child in root.Children)
                foreach (var descendant in Descendants(child))
                    yield return descendant;
        }

        private sealed class IdentifierVisitor : BaseASTVisitor<int>
        {
            public List<string> Identifiers { get; } = new();
            public override int Visit(IdNode node)
            {
                this.Identifiers.Add(node.Identifier);
                return 1;
            }
        }
    }
}
