using System.Linq;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class ClassTests : ASTBuilderTestBase
    {
        protected override ASTNode GenerateAST(string src)
            => new KotlinASTBuilder().BuildFromSource(src, p => p.classDeclaration());

        private TypeDeclNode TypeDecl(string src)
            => this.GenerateAST(src).As<ClassNode>().DeclaratorList.Declarators.Single().As<TypeDeclNode>();

        // --- CLASS / INTERFACE keyword ---

        [Test]
        public void ClassKeywordProducesClassNodeTest()
        {
            Assert.That(this.GenerateAST("class Foo"), Is.InstanceOf<ClassNode>());
        }

        [Test]
        public void InterfaceKeywordProducesInterfaceNodeTest()
        {
            Assert.That(this.GenerateAST("interface Printable { }"), Is.InstanceOf<InterfaceNode>());
        }

        // --- identifier ---

        [Test]
        public void ClassIdentifierTest()
        {
            TypeDeclNode node = TypeDecl("class Person(val name: String)");
            Assert.That(node.Identifier, Is.EqualTo("Person"));
        }

        // --- primaryConstructor ---

        [Test]
        public void PrimaryConstructorSingleParamTest()
        {
            TypeDeclNode node = TypeDecl("class Point(val x: Int)");
            FuncDeclNode ctor = node.Declarations.First().DeclaratorList.Declarators.Single().As<FuncDeclNode>();
            FuncParamNode param = ctor.Parameters!.Single();
            Assert.That(param.Declarator.Identifier, Is.EqualTo("x"));
            Assert.That(param.Specifiers.TypeName, Is.EqualTo("Int"));
        }

        [Test]
        public void PrimaryConstructorMultipleParamsTest()
        {
            TypeDeclNode node = TypeDecl("class Point(val x: Int, val y: Int)");
            FuncDeclNode ctor = node.Declarations.First().DeclaratorList.Declarators.Single().As<FuncDeclNode>();
            var ps = ctor.Parameters!.ToList();
            Assert.That(ps, Has.Exactly(2).Items);
            Assert.That(ps[0].Declarator.Identifier, Is.EqualTo("x"));
            Assert.That(ps[1].Declarator.Identifier, Is.EqualTo("y"));
        }

        [Test]
        public void PrimaryConstructorVarParamTest()
        {
            TypeDeclNode node = TypeDecl("class Counter(var count: Int)");
            FuncDeclNode ctor = node.Declarations.First().DeclaratorList.Declarators.Single().As<FuncDeclNode>();
            FuncParamNode param = ctor.Parameters!.Single();
            Assert.That(param.Declarator.Identifier, Is.EqualTo("count"));
            Assert.That(param.Specifiers.TypeName, Is.EqualTo("Int"));
        }

        // --- delegationSpecifiers ---

        [Test]
        public void SingleBaseTypeTest()
        {
            TypeDeclNode node = TypeDecl("class Dog : Animal");
            Assert.That(node.BaseTypes.Types.Single().TypeName, Is.EqualTo("Animal"));
        }

        // --- classBody ---

        [Test]
        public void EmptyClassNoDeclarationsTest()
        {
            TypeDeclNode node = TypeDecl("class Empty");
            Assert.That(node.Declarations, Is.Empty);
        }

        [Test]
        public void ClassBodyMethodTest()
        {
            TypeDeclNode node = TypeDecl("class Greeter {\nfun hello() { }\n}");
            FuncNode method = node.Declarations.Single().As<FuncNode>();
            Assert.That(method.Identifier, Is.EqualTo("hello"));
        }

        [Test]
        public void ClassBodyPropertyTest()
        {
            TypeDeclNode node = TypeDecl("class Box {\nval size: Int = 0\n}");
            DeclStatNode prop = node.Declarations.Single();
            Assert.That(prop.DeclaratorList.Declarators.Single().Identifier, Is.EqualTo("size"));
        }

        [Test]
        public void ConstructorAndBodyMemberCountTest()
        {
            TypeDeclNode node = TypeDecl("class Counter(var count: Int) {\nval step: Int = 1\n}");
            Assert.That(node.Declarations.Count(), Is.EqualTo(2));
        }
    }
}
