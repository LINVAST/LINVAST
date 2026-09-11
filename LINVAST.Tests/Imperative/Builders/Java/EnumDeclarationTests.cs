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
using System.Linq;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Java
{
    internal sealed class EnumDeclarationTests : DeclarationTestsBase
    {
        [Test]
        public void NoConstantsEnumDeclTest()
        {
            const string src1 = "enum Color {}";
            EnumDeclNode ast1 = this.GenerateAST(src1).As<EnumDeclNode>();
            Assert.That(ast1.Identifier, Is.EqualTo("Color"));
        }

        [Test]
        public void ConstantsEnumDeclTest()
        {
            // jdk/src/java.sql/share/classes/java/sql/ClientInfoStatus.java
            const string src1 = @"
               enum ClientInfoStatus {
                /**
                 * The client info property could not be set for some unknown reason
                 * @since 1.6
                 */
                REASON_UNKNOWN,

                /**
                 * The client info property name specified was not a recognized property
                 * name.
                 * @since 1.6
                 */
                REASON_UNKNOWN_PROPERTY,

                /**
                 * The value specified for the client info property was not valid.
                 * @since 1.6
                 */
                REASON_VALUE_INVALID,

                /**
                 * The value specified for the client info property was too large.
                 * @since 1.6
                 */
                REASON_VALUE_TRUNCATED
            }
            ";
            EnumDeclNode ast1 = this.GenerateAST(src1).As<EnumDeclNode>();
            Assert.That(ast1.Identifier, Is.EqualTo("ClientInfoStatus"));
        }

        [Test]
        public void AnnotationsEnumDeclTest()
        {
            const string src1 = @"
               enum ClientInfoStatus {
                @Foo                
                REASON_UNKNOWN,

                @Bar @Bar REASON_UNKNOWN_PROPERTY,
                
                @Foo @Bar
                REASON_VALUE_INVALID
            }
            ";
            EnumDeclNode ast1 = this.GenerateAST(src1).As<EnumDeclNode>();
            Assert.That(ast1.Identifier, Is.EqualTo("ClientInfoStatus"));
        }

        [Test]
        public void EnumBodyDeclTest()
        {
            const string src1 = @"
               enum ClientInfoStatus {
                @Foo                
                REASON_UNKNOWN;

                ClientInfoStatus() {
                }
            }
            ";
            EnumDeclNode ast1 = this.GenerateAST(src1).As<EnumDeclNode>();
            Assert.That(ast1.Identifier, Is.EqualTo("ClientInfoStatus"));
        }

        [Test]
        public void EnumConstantArgumentsAndImplementsTest()
        {
            const string src1 = "enum Status implements Printable { OK(200), FAIL(500) }";

            EnumDeclNode ast1 = this.GenerateAST(src1).As<EnumDeclNode>();
            VarDeclNode first = ast1.Constants.Declarators.First().As<VarDeclNode>();

            Assert.That(ast1.Identifier, Is.EqualTo("Status"));
            Assert.That(first.Identifier, Is.EqualTo("OK"));
            Assert.That(first.Initializer, Is.InstanceOf<FuncCallExprNode>());
            Assert.That(first.Initializer!.As<FuncCallExprNode>().Identifier, Is.EqualTo("OK"));
        }
        
        protected override ASTNode GenerateAST(string src)
            => new JavaASTBuilder().BuildFromSource(src, p => p.enumDeclaration());
    }
}
