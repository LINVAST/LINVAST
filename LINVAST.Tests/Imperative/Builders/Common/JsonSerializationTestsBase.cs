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


﻿using LINVAST.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Common
{
    internal abstract class JsonSerializationTestsBase : ASTBuilderTestBase
    {
        protected void AssertSerialization(string src)
        {
            ASTNode ast = this.GenerateAST(src);
            string? normal = null;
            string? compact = null;
            Assert.That(() => { normal = ast.ToJson(compact: false); }, Throws.Nothing);
            Assert.That(() => { compact = ast.ToJson(compact: true); }, Throws.Nothing);
            Assert.That(normal, Is.Not.Null);
            Assert.That(compact, Is.Not.Null);
            Assert.That(normal, Has.Length.GreaterThan(compact!.Length));
        }
    }
}

