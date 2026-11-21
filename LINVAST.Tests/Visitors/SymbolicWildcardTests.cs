using System.Linq;
using System.Threading.Tasks;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using NUnit.Framework;

namespace LINVAST.Tests.Visitors
{
    internal sealed class SymbolicWildcardTests
    {
        [Test]
        public void ParallelConversionsHaveStableDistinctNames()
        {
            string[] names = new string[2000];
            Parallel.For(0, names.Length, i => names[i] = new SymbolicExpressionBuilder(Unsupported(i % 100)).Parse().ToString());
            Assert.That(names.Distinct().Count(), Is.EqualTo(100));
            for (int i = 0; i < names.Length; i++)
                Assert.That(names[i], Is.EqualTo(names[i % 100]));
            Assert.That(names.Select(name => SymbolicExpressionBuilder.WildcardReplace(name)), Is.All.EqualTo("?"));
        }

        [Test]
        public void ReusedAndSeparateBuildersAgreeWithoutRetainingExpressionHistory()
        {
            var builder = new SymbolicExpressionBuilder(Unsupported(0));
            string first = builder.Parse().ToString();
            for (int i = 1; i < 100; i++) {
                builder.Node = Unsupported(i);
                Assert.That(builder.Parse().ToString(), Is.Not.EqualTo(first));
            }
            builder.Node = Unsupported(0);
            Assert.That(builder.Parse().ToString(), Is.EqualTo(first));
            Assert.That(new SymbolicExpressionBuilder(Unsupported(0)).Parse().ToString(), Is.EqualTo(first));
        }

        [Test]
        public void ParallelReadsOfOneBuilderAreSafe()
        {
            var builder = new SymbolicExpressionBuilder(Unsupported(123));
            string expected = builder.Parse().ToString();
            Parallel.For(0, 100, _ => Assert.That(builder.Parse().ToString(), Is.EqualTo(expected)));
        }

        [Test]
        public void ReplacementMatchesWholeWildcardNamesAndLegacyNumbers()
        {
            string wildcard = new SymbolicExpressionBuilder(Unsupported(42)).Parse().ToString();
            Assert.That(SymbolicExpressionBuilder.WildcardReplace($"{wildcard} + v__123 + prefixv__123suffix"),
                Is.EqualTo("? + ? + prefixv__123suffix"));
        }

        private static RelExprNode Unsupported(int index)
            => new(1, new IdNode(1, "a" + index), RelOpNode.FromSymbol(1, "<"), new IdNode(1, "b" + index));
    }
}
