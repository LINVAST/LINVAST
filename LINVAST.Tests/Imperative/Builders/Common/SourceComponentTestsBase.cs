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


﻿using LINVAST.Imperative.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Common
{
    internal abstract class SourceComponentTestsBase : ASTBuilderTestBase
    {
        protected SourceNode AssertTranslationUnit(string src, bool empty = false)
        {
            SourceNode sc = this.GenerateAST(src).As<SourceNode>();
            Assert.That(sc, Is.Not.Null);
            Assert.That(sc, Is.InstanceOf<SourceNode>());
            Assert.That(sc.Line, Is.EqualTo(1));
            Assert.That(sc.Parent, Is.Null);
            Assert.That(sc.Children, empty ? Is.Empty : Is.Not.Empty);
            if (!empty)
                Assert.That(sc.Children, Is.Not.All.Null);
            return sc;
        }
    }
}
