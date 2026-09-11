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


﻿using System.Linq;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.C
{
    internal sealed class BlockTests : BlockTestsBase
    {
        [Test]
        public void EmptyBlockTest()
        {
            this.AssertBlock("{ }", empty: true);
            this.AssertBlock(@"{ 
                    // still empty
            }", empty: true);
        }

        [Test]
        public void SimpleBlockTest()
        {
            BlockStatNode block = this.AssertBlock(@" 
                {           // line 2
                            // line 3
                    int x;  // line 4, block begins
                }
            ");
            Assert.That(block.Line, Is.EqualTo(4));
            Assert.That(block.Children.Single(), Is.InstanceOf<DeclStatNode>());
        }

        [Test]
        public void ComplexBlockTest()
        {
            BlockStatNode block = this.AssertBlock(@" 
                {           // line 2
                    int x;  // line 3, block begins
                    if (x) {
                        int z = y + 4;
                        ;
                    } else {
                        float x, y = x + 3;
                        ;
                        ;
                    }

                    float w, h;
                }
            ");
            Assert.That(block.Line, Is.EqualTo(3));
            Assert.That(block.Children, Has.Exactly(3).Items);
            Assert.That(block.Children.ElementAt(0), Is.InstanceOf<DeclStatNode>());
            Assert.That(block.Children.ElementAt(1), Is.InstanceOf<IfStatNode>());
            Assert.That(block.Children.ElementAt(2), Is.InstanceOf<DeclStatNode>());
        }


        protected override ASTNode GenerateAST(string src)
            => new CASTBuilder().BuildFromSource(src, p => p.compoundStatement());
    }
}
