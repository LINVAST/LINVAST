using System;
using System.Linq;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Kotlin
{
    internal sealed class DeclarationTests : ASTBuilderTestBase
    {
        protected override ASTNode GenerateAST(string src)
            => new KotlinASTBuilder().BuildFromSource(src, p => p.propertyDeclaration());

        // --- importHeader tests ---

        [Test]
        public void SimpleImportTest()
        {
            ImportNode node = new KotlinASTBuilder()
                .BuildFromSource("import kotlin.math.sqrt", p => p.importHeader())
                .As<ImportNode>();
            Assert.That(node.Directive, Is.EqualTo("kotlin.math.sqrt"));
        }

        [Test]
        public void SingleSegmentImportTest()
        {
            ImportNode node = new KotlinASTBuilder()
                .BuildFromSource("import kotlin", p => p.importHeader())
                .As<ImportNode>();
            Assert.That(node.Directive, Is.EqualTo("kotlin"));
        }

        [Test]
        public void DeepQualifiedImportTest()
        {
            ImportNode node = new KotlinASTBuilder()
                .BuildFromSource("import java.util.Collections", p => p.importHeader())
                .As<ImportNode>();
            Assert.That(node.Directive, Is.EqualTo("java.util.Collections"));
        }

        // --- propertyDeclaration tests ---

        [Test]
        public void ValTypedDeclarationTest()
        {
            // val x: Int
            DeclStatNode node = this.GenerateAST("val x: Int").As<DeclStatNode>();
            Assert.That(node.Specifiers.TypeName, Is.EqualTo("Int"));
            Assert.That(node.DeclaratorList.Declarators.Single().Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void VarTypedDeclarationTest()
        {
            // var name: String
            DeclStatNode node = this.GenerateAST("var name: String").As<DeclStatNode>();
            Assert.That(node.Specifiers.TypeName, Is.EqualTo("String"));
            Assert.That(node.DeclaratorList.Declarators.Single().Identifier, Is.EqualTo("name"));
        }

        [Test]
        public void ValNullableTypeTest()
        {
            // val name: String?
            DeclStatNode node = this.GenerateAST("val name: String?").As<DeclStatNode>();
            Assert.That(node.Specifiers.TypeName, Is.EqualTo("String?"));
            Assert.That(node.DeclaratorList.Declarators.Single().Identifier, Is.EqualTo("name"));
        }

        // --- variableDeclaration tests ---

        [Test]
        public void VariableDeclarationNoTypeTest()
        {
            // standalone variableDeclaration rule: just the name without val/var keyword
            VarDeclNode node = new KotlinASTBuilder()
                .BuildFromSource("x", p => p.variableDeclaration())
                .As<VarDeclNode>();
            Assert.That(node.Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void VariableDeclarationWithTypeTest()
        {
            // variableDeclaration rule with type annotation
            VarDeclNode node = new KotlinASTBuilder()
                .BuildFromSource("x: Int", p => p.variableDeclaration())
                .As<VarDeclNode>();
            Assert.That(node.Identifier, Is.EqualTo("x"));
        }

        // --- typeAlias tests ---

        [Test]
        public void TypeAliasThrowsTest()
        {
            Assert.That(
                () => new KotlinASTBuilder().BuildFromSource("typealias MyInt = Int", p => p.typeAlias()),
                Throws.InstanceOf<NotImplementedException>());
        }
    }
}
