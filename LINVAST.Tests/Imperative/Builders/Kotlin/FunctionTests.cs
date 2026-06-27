using System;
using System.Linq;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class FunctionTests : ASTBuilderTestBase
    {
        protected override ASTNode GenerateAST(string src)
            => new KotlinASTBuilder().BuildFromSource(src, p => p.functionDeclaration());

        // --- parameter ---

        [Test]
        public void ParameterNameAndTypeTest()
        {
            FuncParamNode node = new KotlinASTBuilder()
                .BuildFromSource("x: Int", p => p.parameter())
                .As<FuncParamNode>();
            Assert.That(node.Specifiers.TypeName, Is.EqualTo("Int"));
            Assert.That(node.Declarator.Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void ParameterNullableTypeTest()
        {
            FuncParamNode node = new KotlinASTBuilder()
                .BuildFromSource("name: String?", p => p.parameter())
                .As<FuncParamNode>();
            Assert.That(node.Specifiers.TypeName, Is.EqualTo("String?"));
            Assert.That(node.Declarator.Identifier, Is.EqualTo("name"));
        }

        // --- functionValueParameters ---

        [Test]
        public void EmptyParameterListTest()
        {
            FuncParamsNode node = new KotlinASTBuilder()
                .BuildFromSource("()", p => p.functionValueParameters())
                .As<FuncParamsNode>();
            Assert.That(node.Parameters, Is.Empty);
        }

        [Test]
        public void SingleFunctionValueParameterTest()
        {
            FuncParamsNode node = new KotlinASTBuilder()
                .BuildFromSource("(x: Int)", p => p.functionValueParameters())
                .As<FuncParamsNode>();
            FuncParamNode param = node.Parameters.Single();
            Assert.That(param.Specifiers.TypeName, Is.EqualTo("Int"));
            Assert.That(param.Declarator.Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void MultipleFunctionValueParametersTest()
        {
            FuncParamsNode node = new KotlinASTBuilder()
                .BuildFromSource("(x: Int, y: String)", p => p.functionValueParameters())
                .As<FuncParamsNode>();
            var paramList = node.Parameters.ToList();
            Assert.That(paramList, Has.Exactly(2).Items);
            Assert.That(paramList[0].Specifiers.TypeName, Is.EqualTo("Int"));
            Assert.That(paramList[0].Declarator.Identifier, Is.EqualTo("x"));
            Assert.That(paramList[1].Specifiers.TypeName, Is.EqualTo("String"));
            Assert.That(paramList[1].Declarator.Identifier, Is.EqualTo("y"));
        }

        // --- functionDeclaration ---

        [Test]
        public void NoParamsNoReturnTypeTest()
        {
            FuncNode node = this.GenerateAST("fun greet() { }").As<FuncNode>();
            Assert.That(node.Identifier, Is.EqualTo("greet"));
            Assert.That(node.ReturnTypeName, Is.EqualTo("Unit"));
            Assert.That(node.Parameters, Is.Empty);
        }

        [Test]
        public void ExplicitReturnTypeTest()
        {
            FuncNode node = this.GenerateAST("fun square(x: Int): Int { }").As<FuncNode>();
            Assert.That(node.Identifier, Is.EqualTo("square"));
            Assert.That(node.ReturnTypeName, Is.EqualTo("Int"));
        }

        [Test]
        public void SingleParamFunctionTest()
        {
            FuncNode node = this.GenerateAST("fun square(x: Int): Int { }").As<FuncNode>();
            FuncParamNode param = node.Parameters!.Single();
            Assert.That(param.Specifiers.TypeName, Is.EqualTo("Int"));
            Assert.That(param.Declarator.Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void MultipleParamsFunctionTest()
        {
            FuncNode node = this.GenerateAST("fun add(x: Int, y: Int): Int { }").As<FuncNode>();
            var paramList = node.Parameters!.ToList();
            Assert.That(paramList, Has.Exactly(2).Items);
            Assert.That(paramList[0].Declarator.Identifier, Is.EqualTo("x"));
            Assert.That(paramList[1].Declarator.Identifier, Is.EqualTo("y"));
        }

        [Test]
        public void FunctionWithReturnTest()
        {
            FuncNode node = this.GenerateAST("fun identity(x: Int): Int { return x }").As<FuncNode>();
            Assert.That(node.Definition, Is.Not.Null);
            JumpStatNode ret = node.Definition!.Children.Single().As<JumpStatNode>();
            Assert.That(ret.ReturnExpr, Is.Not.Null);
            Assert.That(ret.ReturnExpr!.As<IdNode>().Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void FunctionBodyStatementCountTest()
        {
            FuncNode node = this.GenerateAST(@"fun add(x: Int, y: Int): Int {
                    val z: Int = 42
                    return z
                }").As<FuncNode>();
            Assert.That(node.Definition, Is.Not.Null);
            Assert.That(node.Definition!.Children, Has.Exactly(2).Items);
        }

        [Test]
        public void FunctionEmptyBodyTest()
        {
            FuncNode node = this.GenerateAST("fun noop() { }").As<FuncNode>();
            Assert.That(node.Definition, Is.Not.Null);
            Assert.That(node.Definition!.Children, Is.Empty);
        }

        [Test]
        public void ExpressionBodyThrowsTest()
        {
            Assert.That(
                () => this.GenerateAST("fun double(x: Int): Int = x * 2"),
                Throws.InstanceOf<NotImplementedException>());
        }
    }
}
