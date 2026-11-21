using System.Linq;
using LINVAST.Imperative.Builders.Python;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Python
{
    internal sealed class LoweringInvariantTests
    {
        [TestCase("f([x for x in xs])\n")]
        [TestCase("a = b = f()\n")]
        public void ReusingABuilderProducesTheSameTree(string source)
        {
            var builder = new PythonASTBuilder();
            ASTNode first = builder.BuildFromSource(source);
            ASTNode second = builder.BuildFromSource(source);
            Assert.That(second, Is.EqualTo(first));
            Assert.That(second.ToJson(), Is.EqualTo(first.ToJson()));
            LoopInvariantTests.AssertOwnedTree(first);
            LoopInvariantTests.AssertOwnedTree(second);
        }

        [Test]
        public void ComprehensionTemporariesAvoidUserNamesAnywhereInSource()
        {
            var tree = new PythonASTBuilder().BuildFromSource(
                "__linvast_comp_0 = 99\nf([x for x in xs])\n__linvast_comp_1 = 100\n");
            var declarations = tree.Children.OfType<DeclStatNode>()
                .SelectMany(d => d.DeclaratorList.Declarators).OfType<VarDeclNode>().ToArray();
            Assert.That(declarations.Select(d => d.Identifier), Is.Unique);
            Assert.That(declarations.Single(d => d.Initializer is ArrInitExprNode).Identifier,
                Is.EqualTo("__linvast_comp_2"));
            LoopInvariantTests.AssertOwnedTree(tree);
        }

        [Test]
        public void ChainedAssignmentEvaluatesRhsOnceAndAssignsLeftToRight()
        {
            var tree = new PythonASTBuilder().BuildFromSource("a = b = f()\n");
            var decls = tree.Children.Cast<DeclStatNode>()
                .Select(d => d.DeclaratorList.Declarators.Single().As<VarDeclNode>()).ToArray();
            Assert.That(decls.Length, Is.EqualTo(3));
            Assert.That(decls[0].Initializer, Is.TypeOf<FuncCallExprNode>());
            Assert.That(decls[1].Identifier, Is.EqualTo("a"));
            Assert.That(decls[2].Identifier, Is.EqualTo("b"));
            Assert.That(decls[1].Initializer!.As<IdNode>().Identifier, Is.EqualTo(decls[0].Identifier));
            Assert.That(decls[2].Initializer!.As<IdNode>().Identifier, Is.EqualTo(decls[0].Identifier));
            Assert.That(Descendants(tree).OfType<FuncCallExprNode>().Count(), Is.EqualTo(1));
            LoopInvariantTests.AssertOwnedTree(tree);
        }

        [Test]
        public void ChainedAssignmentTemporaryAvoidsExistingNames()
        {
            var tree = new PythonASTBuilder().BuildFromSource("__linvast_assign_0 = 7\na = b = f()\n");
            var decls = tree.Children.Cast<DeclStatNode>().SelectMany(d => d.DeclaratorList.Declarators).ToArray();
            Assert.That(decls.Select(d => d.Identifier), Is.Unique);
            Assert.That(decls[1].Identifier, Is.EqualTo("__linvast_assign_1"));
        }

        [Test]
        public void CustomEntryPointAlsoResetsTemporaryNames()
        {
            var builder = new PythonASTBuilder();
            var first = builder.BuildFromSource("a = b = f()", p => p.expr_stmt());
            Assert.That(builder.BuildFromSource("a = b = f()", p => p.expr_stmt()), Is.EqualTo(first));
        }

        private static System.Collections.Generic.IEnumerable<ASTNode> Descendants(ASTNode node)
            => node.Children.SelectMany(c => new[] { c }.Concat(Descendants(c)));
    }
}
