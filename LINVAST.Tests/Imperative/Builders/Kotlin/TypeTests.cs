using System.Linq;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class TypeTests
    {
        private static TypeNameNode ParseType(string source)
            => new KotlinASTBuilder().BuildFromSource(source, p => p.type()).As<TypeNameNode>();

        [TestCase("Int", "int")]
        [TestCase("Long", "long")]
        [TestCase("Short", "short")]
        [TestCase("Byte", "byte")]
        [TestCase("Float", "float")]
        [TestCase("Double", "double")]
        [TestCase("Boolean", "bool")]
        [TestCase("Char", "char")]
        [TestCase("String", "string")]
        [TestCase("Any", "object")]
        [TestCase("Unit", "void")]
        [TestCase("kotlin.Int", "int")]
        [TestCase("kotlin.Unit", "void")]
        [TestCase("Int?", "int?")]
        [TestCase("(Int)", "int")]
        [TestCase("(Int)?", "int?")]
        [TestCase("Array<Int>", "int[]")]
        [TestCase("kotlin.Array<kotlin.Int>", "int[]")]
        [TestCase("Array<Array<Int>>", "int[][]")]
        [TestCase("Array<Int?>", "int?[]")]
        [TestCase("Array<Int>?", "int[]?")]
        [TestCase("Array<Int?>?", "int?[]?")]
        [TestCase("Array<IntArray>", "int[][]")]
        [TestCase("IntArray", "int[]")]
        [TestCase("LongArray", "long[]")]
        [TestCase("ShortArray", "short[]")]
        [TestCase("ByteArray", "byte[]")]
        [TestCase("FloatArray", "float[]")]
        [TestCase("DoubleArray", "double[]")]
        [TestCase("BooleanArray", "bool[]")]
        [TestCase("CharArray", "char[]")]
        [TestCase("IntArray?", "int[]?")]
        [TestCase("Widget", "Widget")]
        [TestCase("example.Int", "example.Int")]
        [TestCase("example.Unit", "example.Unit")]
        [TestCase("() -> Unit", "() -> void")]
        [TestCase("(Int, Array<Int>) -> Unit", "(int, int[]) -> void")]
        [TestCase("String.(value: Int) -> Array<Int>", "string.(value: int) -> int[]")]
        [TestCase("((Int) -> Unit)?", "((int) -> void)?")]
        [TestCase("Array<() -> Unit>", "(() -> void)[]")]
        [TestCase("Array<(() -> Unit)?>", "(() -> void)?[]")]
        public void NormalizedTypeNameTest(string source, string expected)
        {
            Assert.That(ParseType(source).TypeName, Is.EqualTo(expected));
        }

        [Test]
        public void NestedTemplateArgumentsTest()
        {
            var type = ParseType("Map<String, List<Array<Int>>>");
            Assert.That(type.TypeName, Is.EqualTo("Map"));
            var arguments = type.TemplateArguments.ToArray();
            Assert.That(arguments, Has.Length.EqualTo(2));
            Assert.That(arguments[0].TypeName, Is.EqualTo("string"));
            Assert.That(arguments[1].TypeName, Is.EqualTo("List"));
            Assert.That(arguments[1].TemplateArguments.Single().TypeName, Is.EqualTo("int[]"));
        }

        [TestCase("List<Int>?", "List?")]
        [TestCase("(List<Int>)?", "List?")]
        [TestCase("Array<List<Int>>", "List[]")]
        [TestCase("example.Array<Int>", "example.Array")]
        [TestCase("example.List<Int>", "example.List")]
        public void WrappersAndQualificationPreserveTemplateArgumentsTest(string source, string expected)
        {
            var type = ParseType(source);
            Assert.That(type.TypeName, Is.EqualTo(expected));
            Assert.That(type.TemplateArguments.Single().TypeName, Is.EqualTo("int"));
        }

        [Test]
        public void QualifiedGenericTypesPreserveOuterArgumentsTest()
        {
            var type = ParseType("Outer<Int>.Inner<String>");
            Assert.That(type.TypeName, Is.EqualTo("Outer<int>.Inner"));
            Assert.That(type.TemplateArguments.Single().TypeName, Is.EqualTo("string"));
        }

        [TestCase("List<*>", "*")]
        [TestCase("List<out Int>", "out int")]
        [TestCase("List<in Int>", "in int")]
        public void TypeProjectionTest(string source, string expected)
        {
            Assert.That(ParseType(source).TemplateArguments.Single().TypeName, Is.EqualTo(expected));
        }

        [TestCase("fun values(input: Array<Int>): Array<Int> {}", "int[]")]
        [TestCase("fun String.values(input: Array<Int>): Array<Int> {}", "int[]")]
        [TestCase("fun values(input: Array<Int>): Unit {}", "void")]
        [TestCase("fun values(input: Array<Int>) {}", "void")]
        public void FunctionSignatureTest(string source, string returnType)
        {
            var function = new KotlinASTBuilder().BuildFromSource(source, p => p.functionDeclaration()).As<FuncNode>();
            Assert.That(function.Identifier, Is.EqualTo("values"));
            Assert.That(function.ReturnTypeName, Is.EqualTo(returnType));
            Assert.That(function.Parameters!.Single().Specifiers.TypeName, Is.EqualTo("int[]"));
        }

        [Test]
        public void DeclarationsPreserveGenericTypesTest()
        {
            var source = new KotlinASTBuilder().BuildFromSource(@"
                val values: List<Array<Int>>
                typealias Values = List<Array<Int>>
                fun values(input: List<Array<Int>>): List<Array<Int>> {}
                class ValuesHolder(val values: List<Array<Int>>)
            ").As<SourceNode>();
            var declarations = source.Children.ToArray();
            var function = declarations[2].As<FuncNode>();
            var constructor = declarations[3].As<ClassNode>().DeclaratorList.Declarators.Single()
                .As<TypeDeclNode>().Declarations.Single()
                .DeclaratorList.Declarators.Single().As<FuncDeclNode>();
            var types = new[] {
                declarations[0].As<DeclStatNode>().Specifiers.TypeNode,
                declarations[1].As<DeclStatNode>().Specifiers.TypeNode,
                function.Specifiers.TypeNode,
                function.Parameters!.Single().Specifiers.TypeNode,
                constructor.Parameters!.Single().Specifiers.TypeNode,
            };
            foreach (var type in types) {
                Assert.That(type.TypeName, Is.EqualTo("List"));
                Assert.That(type.TemplateArguments.Single().TypeName, Is.EqualTo("int[]"));
            }
        }
    }
}
