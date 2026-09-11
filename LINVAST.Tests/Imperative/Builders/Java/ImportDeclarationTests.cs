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


﻿using System;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Java
{
    internal sealed class ImportDeclarationTests : DeclarationTestsBase
    {
        [Test]
        public void SingleNameImportDeclTest()
        {
            string src1 = "import system;";
            string src2 = "import system ;";

            ImportNode ast1 = this.GenerateAST(src1).As<ImportNode>();
            ImportNode ast2 = this.GenerateAST(src2).As<ImportNode>();

            Assert.That(ast1.Directive, Is.EqualTo("system"));
            Assert.That(ast2.Directive, Is.EqualTo("system"));
        }

        [Test]
        public void WithDotsImportDeclTest()
        {
            string src1 = "import name1.name2.name3.name4;";
            string src2 = "import system.text.json;";
            string src3 = "import system.text ;";

            ImportNode ast1 = this.GenerateAST(src1).As<ImportNode>();
            ImportNode ast2 = this.GenerateAST(src2).As<ImportNode>();
            ImportNode ast3 = this.GenerateAST(src3).As<ImportNode>();

            Assert.That(ast1.Directive, Is.EqualTo("name1.name2.name3.name4"));
            Assert.That(ast2.Directive, Is.EqualTo("system.text.json"));
            Assert.That(ast3.Directive, Is.EqualTo("system.text"));
        }

        [Test]
        public void WithWildcardImportDeclTest()
        {
            string src1 = "import system.text.json.*;";
            string src2 = "import system.text. *;";
            string src3 = "import system.utils.*;";

            ImportNode ast1 = this.GenerateAST(src1).As<ImportNode>();
            ImportNode ast2 = this.GenerateAST(src2).As<ImportNode>();
            ImportNode ast3 = this.GenerateAST(src3).As<ImportNode>();

            Assert.That(ast1.Directive, Is.EqualTo("system.text.json.*"));
            Assert.That(ast2.Directive, Is.EqualTo("system.text.*"));
            Assert.That(ast3.Directive, Is.EqualTo("system.utils.*"));
        }

        [Test]
        public void StaticImportDeclTest()
        {
            string src1 = "import static system.text.json.*;";
            string src2 = "import static system;";
            string src3 = "import static system.text;";

            Assert.That(() => this.GenerateAST(src1), Throws.Nothing);
            Assert.That(() => this.GenerateAST(src2), Throws.Nothing);
            Assert.That(() => this.GenerateAST(src3), Throws.Nothing);
        }


        protected override ASTNode GenerateAST(string src)
            => new JavaASTBuilder().BuildFromSource(src, p => p.importDeclaration());
    }
}
