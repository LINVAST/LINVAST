﻿using System;
using System.Linq;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Java
{
    internal sealed class ClassBodyDeclarationTests : DeclarationTestsBase
    {

        [Test]
        public void EmptyClassBodyDeclTest()
        {
            string src1 = ";";

            EmptyStatNode ast1 = this.GenerateAST(src1).As<EmptyStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo(";"));
        }


        [Test]
        public void StaticBlockClassBodyDeclTest()
        {
            string src1 = "static {}";
            string src2 = "static {x=3;}";

            Assert.That(() => this.GenerateAST(src1), Throws.InstanceOf<NotImplementedException>());
            Assert.That(() => this.GenerateAST(src2), Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void NonStaticBlockClassBodyDeclTest()
        {
            string src1 = "{}";
            string src2 = "{x=3;}";

            Assert.That(() => this.GenerateAST(src1), Throws.InstanceOf<NotImplementedException>());
            Assert.That(() => this.GenerateAST(src2), Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void WithAnnotationClassBodyDeclTest()
        {
            string src1 = @"@Override 
                            public string toString() 
                            {
                                return this.attr.toString();
                            }";

            Assert.That(() => this.GenerateAST(src1), Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void WithoutInitializerFieldDeclarationTest()
        {
            string src1 = "String x;";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo("String x;"));
        }

        [Test]
        public void WithInitializerFieldDeclarationTest()
        {
            string src1 = "String x = null;";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo("String x = null;"));
        }

        [Test]
        public void MultipleInitializersFieldDeclarationTest()
        {
            string src1 = "String x = null, y, z;";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo("String x = null, y, z;"));
            Assert.That(ast1.DeclaratorList.Children.Count, Is.EqualTo(3));
            Assert.That(ast1.DeclaratorList.Declarators.First().Identifier, Is.EqualTo("x"));
        }

        [Test]
        public void StartLineFieldDeclarationTest()
        {
            string src1 = @"String x = null, // comment for x
                                   y, // comment for y
                                   z; // comment for z";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo("String x = null, y, z;"));
            Assert.That(ast1.DeclaratorList.Children.Count, Is.EqualTo(3));
            Assert.That(ast1.DeclaratorList.Declarators.First().Identifier, Is.EqualTo("x"));
            Assert.That(ast1.DeclaratorList.Declarators.Last().Line, Is.EqualTo(3));
        }

        [Test]
        public void SingleModifierFieldDeclarationTest()
        {
            string src1 = "private String x = null;";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo("private String x = null;"));
            Assert.That(ast1.Specifiers.Modifiers.ToString(), Is.EqualTo("private"));
        }

        [Test]
        public void MultipleModifiersFieldDeclarationTest()
        {
            string src1 = "public static String x = null;";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            Assert.That(ast1.GetText(), Is.EqualTo("public static String x = null;"));
            Assert.That(ast1.Specifiers.Modifiers.ToString, Is.EqualTo("public static"));
        }

        [Test]
        public void StatLineWithModifiersFieldDeclTest()
        {
            string src1 = @"private 
                            String x = null;";

            string src2 = @"private 
                            static
                            String x = null;";

            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            DeclStatNode ast2 = this.GenerateAST(src2).As<DeclStatNode>();

            Assert.That(ast1.GetText(), Is.EqualTo("private String x = null;"));
            Assert.That(ast1.Modifiers.ToString(), Is.EqualTo("private"));
            Assert.That(ast1.Specifiers.Line, Is.EqualTo(1));
            Assert.That(ast1.DeclaratorList.Declarators.First().Line, Is.EqualTo(2));

            Assert.That(ast2.GetText(), Is.EqualTo("private static String x = null;"));
            Assert.That(ast2.Modifiers.ToString(), Is.EqualTo("private static"));
            Assert.That(ast2.Specifiers.Line, Is.EqualTo(1));
            Assert.That(ast2.DeclaratorList.Declarators.First().Line, Is.EqualTo(3));
        }

        [Test]
        public void MethodDeclarationTest()
        {
            string src1 = "String f() {}";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            StringAssert.IsMatch(@"String f\(<>\){\s*}", ast1.GetText());
            Assert.That(ast1.Specifiers.TypeName, Is.EqualTo("String"));
        }


        [Test]
        public void WithModifiersMethodDeclarationTest()
        {
            string src1 = "public static String f() {}";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            StringAssert.IsMatch(@"public static String f\(<>\){\s*}", ast1.GetText());
        }

        [Test]
        public void GenericMethodDeclarationTest()
        {
            string src1 = "<Class1> String f() {}";
            string src2 = "public <Class1> String f() {}";
            DeclStatNode ast1 = this.GenerateAST(src1).As<DeclStatNode>();
            DeclStatNode ast2 = this.GenerateAST(src2).As<DeclStatNode>();
            StringAssert.IsMatch(@"String f\(<Class1>\){\s*}", ast1.GetText());
            StringAssert.IsMatch(@"public String f\(<Class1>\){\s*}", ast2.GetText());
            Assert.That(ast2.Modifiers.ToString(), Is.EqualTo("public"));
        }

        [Test]
        public void ConstructorDeclarationTest()
        {
            string src1 = "Class1 () {}";
            Assert.That(() => this.GenerateAST(src1), Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void GenericConstructorDeclarationTest()
        {
            string src1 = "<Type1> Class1 () {}";
            Assert.That(() => this.GenerateAST(src1), Throws.InstanceOf<NotImplementedException>());
        }

        [Test]
        public void AnnotationTypeDeclarationTest()
        {
            string src1 = "@interface Id1 {}";
            Assert.That(() => this.GenerateAST(src1), Throws.InstanceOf<NotImplementedException>());
        }



        protected override ASTNode GenerateAST(string src)
            => new JavaASTBuilder().BuildFromSource(src, p => p.classBodyDeclaration());
    }
}
