using System.Linq;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Builders.Python;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Common
{
    internal sealed class TypeNormalizationTests
    {
        [TestCase("boolean", "bool")]
        [TestCase("boolean[][]", "bool[][]")]
        [TestCase("String[]", "string[]")]
        [TestCase("java.lang.String[]", "string[]")]
        [TestCase("Object", "object")]
        [TestCase("java.lang.Object", "object")]
        [TestCase("example.String", "example.String")]
        [TestCase("Integer", "Integer")]
        public void JavaBuiltinsTest(string source, string expected)
        {
            var type = new JavaASTBuilder().BuildFromSource(source, p => p.typeType()).As<TypeNameNode>();
            Assert.That(type.TypeName, Is.EqualTo(expected));
        }

        [TestCase("Map<String, List<boolean[]>>", "Map<string, List<bool[]>>")]
        [TestCase("List<String>[]", "List<string>[]")]
        [TestCase("Outer<String>.Inner<boolean[]>", "Outer<string>.Inner<bool[]>")]
        [TestCase("java.util.List<String>", "java.util.List<string>")]
        public void JavaGenericsTest(string source, string expected)
        {
            var type = new JavaASTBuilder().BuildFromSource(source, p => p.typeType()).As<TypeNameNode>();
            Assert.That(type.GetText(), Is.EqualTo(expected));
            Assert.That(type.TemplateArguments, Is.Not.Empty);
        }

        [Test]
        public void JavaCreatedTypePreservesQualificationTest()
        {
            var type = new JavaASTBuilder().BuildFromSource("java.util.List<String>", p => p.createdName()).As<TypeNameNode>();
            Assert.That(type.TypeName, Is.EqualTo("java.util.List"));
            Assert.That(type.TemplateArguments.Single().TypeName, Is.EqualTo("string"));
        }

        [TestCase("str", "string")]
        [TestCase("builtins.str", "string")]
        [TestCase("Any", "object")]
        [TestCase("typing.Any", "object")]
        [TestCase("list[str]", "list[string]")]
        [TestCase("dict[str,list[str]]", "dict[string,list[string]]")]
        [TestCase("str | None", "string|None")]
        [TestCase("custom.str", "custom.str")]
        [TestCase("Literal['str']", "Literal['str']")]
        public void PythonAnnotationsTest(string annotation, string expected)
        {
            var function = new PythonASTBuilder().BuildFromSource($"def f(value: {annotation}) -> {annotation}:\n    pass\n", p => p.funcdef()).As<FuncNode>();
            Assert.That(function.ReturnTypeName, Is.EqualTo(expected));
            Assert.That(function.Parameters!.Single().Specifiers.TypeName, Is.EqualTo(expected));
            var variable = new PythonASTBuilder().BuildFromSource($"value: {annotation}", p => p.expr_stmt()).As<DeclStatNode>();
            Assert.That(variable.Specifiers.TypeName, Is.EqualTo(expected));
        }

        [Test]
        public void PythonNoneReturnTypeTest()
        {
            var function = new PythonASTBuilder().BuildFromSource("def f() -> None:\n    pass\n", p => p.funcdef()).As<FuncNode>();
            Assert.That(function.ReturnTypeName, Is.EqualTo("void"));
        }

        [TestCase("_Bool", "bool")]
        public void CBooleanTypeTest(string source, string expected)
        {
            var type = new CASTBuilder().BuildFromSource(source, p => p.typeSpecifier()).As<TypeNameNode>();
            Assert.That(type.TypeName, Is.EqualTo(expected));
            var declaration = new CASTBuilder().BuildFromSource(source + " value;", p => p.declaration()).As<DeclStatNode>();
            Assert.That(declaration.Specifiers.TypeName, Is.EqualTo(expected));
        }

        [TestCase("List[]", "List<int>[]")]
        [TestCase("List?[]", "List<int>?[]")]
        [TestCase("List[][]?", "List<int>[][]?")]
        public void GenericTypeSuffixRenderingTest(string name, string expected)
        {
            var type = new TypeNameNode(1, name, new TypeNameNode(1, "int"));
            Assert.That(type.GetText(), Is.EqualTo(expected));
        }
    }
}
