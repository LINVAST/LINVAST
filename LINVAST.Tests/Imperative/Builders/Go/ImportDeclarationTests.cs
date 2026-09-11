// LINVAST - Language-INVariant AST library
// Copyright (C) 2026 Ivan Ristović
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.


using System;
using System.Linq;
using LINVAST.Imperative.Builders.Go;
using LINVAST.Nodes;
using LINVAST.Imperative.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;


namespace LINVAST.Tests.Imperative.Builders.Go
{
    internal sealed class ImportDeclarationTests : DeclarationTestsBase
    {
        [Test]
        public void SimpleImportDeclarationTest()
        {
            string src1 = "import \"lib/math\"";
            string src2 = "import \"fmt\"";
            
            ImportNode ast1 = this.GenerateAST(src1).Children.First().As<ImportNode>();
            ImportNode ast2 = this.GenerateAST(src2).Children.First().As<ImportNode>();

            Assert.That(ast1.Directive, Is.EqualTo("\"lib/math\""));
            Assert.That(ast2.Directive, Is.EqualTo("\"fmt\""));
        }

        [Test]
        public void ImportDeclarationWithIdentifierTest()
        {
            string src1 = "import lm \"lib/math\"";
            string src2 = "import f \"fmt\"";

            ImportNode ast1 = this.GenerateAST(src1).Children.First().As<ImportNode>();
            ImportNode ast2 = this.GenerateAST(src2).Children.First().As<ImportNode>();

            Assert.That(ast1.Directive, Is.EqualTo("\"lib/math\""));
            Assert.That(ast1.QualifiedAs, Is.EqualTo("lm"));

            Assert.That(ast2.Directive, Is.EqualTo("\"fmt\""));
            Assert.That(ast2.QualifiedAs, Is.EqualTo("f"));
            
        }

        [Test]
        public void ImportDeclarationWithDotTest()
        {
            string src1 = "import . \"lib/math\"";
            string src2 = "import . \"fmt\"";

            ImportNode ast1 = this.GenerateAST(src1).Children.First().As<ImportNode>();
            ImportNode ast2 = this.GenerateAST(src2).Children.First().As<ImportNode>();

            Assert.That(ast1.Directive, Is.EqualTo("\"lib/math\""));
            Assert.That(ast1.QualifiedAs, Is.EqualTo(""));

            Assert.That(ast2.Directive, Is.EqualTo("\"fmt\""));
            Assert.That(ast2.QualifiedAs, Is.EqualTo(""));

        }
        
        [Test]
        public void ImportDeclarationListTest()
        {
            string src1 = "import (" +
                         "\"lib/math\" \n" +
                         "\"fmt\" \n" +
                         ")";
            
            string src2 = "import (" +
                          ". \"lib/math\" \n" +
                          "f \"fmt\" \n" +
                          ")";

            ImportListNode ast1 = this.GenerateAST(src1).As<ImportListNode>();
            ImportListNode ast2 = this.GenerateAST(src2).As<ImportListNode>();
            
            Assert.That(ast1.Children[0].As<ImportNode>().Directive, Is.EqualTo("\"lib/math\""));
            Assert.That(ast1.Children[1].As<ImportNode>().Directive, Is.EqualTo("\"fmt\""));
            
            Assert.That(ast2.Children[0].As<ImportNode>().Directive, Is.EqualTo("\"lib/math\""));
            Assert.That(ast2.Children[0].As<ImportNode>().QualifiedAs, Is.EqualTo(""));

            Assert.That(ast2.Children[1].As<ImportNode>().Directive, Is.EqualTo("\"fmt\""));
            Assert.That(ast2.Children[1].As<ImportNode>().QualifiedAs, Is.EqualTo("f"));
        }
        
        protected override ASTNode GenerateAST(string src)
            => new GoASTBuilder().BuildFromSource(src, p => p.importDecl());
    }
}