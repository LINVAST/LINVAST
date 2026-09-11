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


﻿using LINVAST.Exceptions;
using LINVAST.Imperative.Builders.Lua;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Lua
{
    internal sealed class BuildingErrorTests : BuildingErrorTestsBase
    {
        [Test]
        public void EmptySourceTest()
        {
            this.AssertThrows<SyntaxErrorException>("");
        }

        // TODO


        protected override ASTNode GenerateAST(string src)
            => new LuaASTBuilder().BuildFromSource(src);
    }
}
