using System.Linq;
using LINVAST.Imperative.Builders.Go;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Go
{
    internal sealed class TypeTests
    {
        [TestCase("[]int", "int[]")]
        [TestCase("[][]int", "int[][]")]
        [TestCase("[3]int", "int[3]")]
        [TestCase("[2][3]int", "int[2][3]")]
        [TestCase("[][3]int", "int[][3]")]
        [TestCase("[3][]int", "int[3][]")]
        [TestCase("[N+1]int", "int[N+1]")]
        [TestCase("[]([]int)", "int[][]")]
        [TestCase("[]pkg.Value", "pkg.Value[]")]
        [TestCase("[]*int", "int*[]")]
        [TestCase("*[]int", "int[]*")]
        [TestCase("map[string][]int", "map[string]int[]")]
        [TestCase("[]map[string]int", "(map[string]int)[]")]
        [TestCase("chan []int", "chan int[]")]
        [TestCase("chan<- []int", "chan<- int[]")]
        [TestCase("<-chan []int", "<-chan int[]")]
        [TestCase("[]chan int", "(chan int)[]")]
        [TestCase("func([]int) []int", "(int[]) -> int[]")]
        [TestCase("func(...[]int)", "(...int[]) -> void")]
        [TestCase("[]func([]int) []int", "((int[]) -> int[])[]")]
        [TestCase("int8", "signed byte")]
        [TestCase("int16", "short")]
        [TestCase("int32", "int")]
        [TestCase("int64", "long")]
        [TestCase("uint8", "byte")]
        [TestCase("uint16", "unsigned short")]
        [TestCase("uint32", "unsigned int")]
        [TestCase("uint64", "unsigned long")]
        [TestCase("float32", "float")]
        [TestCase("float64", "double")]
        [TestCase("rune", "int")]
        [TestCase("any", "object")]
        [TestCase("interface{}", "object")]
        [TestCase("Widget", "Widget")]
        [TestCase("pkg.int32", "pkg.int32")]
        public void TypeNameTest(string source, string expected)
        {
            var type = new GoASTBuilder().BuildFromSource(source, p => p.type_()).As<TypeNameNode>();
            Assert.That(type.TypeName, Is.EqualTo(expected));
        }

        [TestCase("[]int{1, 2}", "int[]")]
        [TestCase("[2]int{1, 2}", "int[2]")]
        [TestCase("[...]int{1, 2}", "int[...]")]
        [TestCase("[][2]int{{1, 2}}", "int[][2]")]
        [TestCase("[]int(xs)", "int[]")]
        public void LiteralAndConversionTypeTest(string source, string expected)
        {
            var expression = new GoASTBuilder().BuildFromSource(source, p => p.expression()).As<ConsExprNode>();
            Assert.That(expression.Identifier, Is.EqualTo(expected));
        }

        [TestCase("make([]int, 3)", "int[]")]
        [TestCase("new([3]int)", "int[3]")]
        [TestCase("make(map[string][]int)", "map[string]int[]")]
        public void AllocationTypeTest(string source, string expected)
        {
            var call = new GoASTBuilder().BuildFromSource(source, p => p.expression()).As<FuncCallExprNode>();
            Assert.That(call.Arguments!.Expressions.First().As<IdNode>().Identifier, Is.EqualTo(expected));
        }

        [Test]
        public void DeclarationAndSignatureTest()
        {
            var declaration = new GoASTBuilder().BuildFromSource("var values []int", p => p.declaration()).As<DeclStatNode>();
            Assert.That(declaration.Specifiers.TypeName, Is.EqualTo("int[]"));

            var alias = new GoASTBuilder().BuildFromSource("type Values = []int", p => p.declaration()).As<DeclStatNode>();
            Assert.That(alias.Specifiers.TypeName, Is.EqualTo("int[]"));

            var function = new GoASTBuilder().BuildFromSource("func values(input []int) ([]int, error) {}", p => p.functionDecl()).As<FuncNode>();
            Assert.That(function.ReturnTypeName, Is.EqualTo("(int[], error)"));
            Assert.That(function.Parameters!.Single().Specifiers.TypeName, Is.EqualTo("int[]"));
        }

        [TestCase("struct { Values []int; }")]
        [TestCase("interface { Values([]int) []int; }")]
        public void AggregateMembersNormalizeNestedTypesTest(string source)
        {
            var type = new GoASTBuilder().BuildFromSource(source, p => p.type_()).As<TypeNameNode>();
            Assert.That(type.TypeName, Does.Contain("int[]"));
            Assert.That(type.TypeName, Does.Not.Contain("[]int"));
        }
    }
}
