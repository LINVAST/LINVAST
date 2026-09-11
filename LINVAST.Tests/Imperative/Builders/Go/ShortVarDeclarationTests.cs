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
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Go
{
    internal sealed class ShortVarDeclarationTests : DeclarationTestsBase
    { 
        
        [Test]
      public void ShortVarDeclarationTest()
      {
          this.AssertVariableDeclaration("i:= 3", "i", "Int64", value: 3 );
          this.AssertVariableDeclaration("s:= \"str\" ", "s", "String", value: "str" );
          
          this.AssertVariableDeclarationList("i, s := 3, \"str\" ", "object", AccessModifiers.Unspecified,
              QualifierFlags.None, ("i", 3), ("s", "str"));
          
          DeclStatNode funcDecl = this.AssertDeclarationNode("f := func() int { return 7 }", "object");
          Assert.That(funcDecl.DeclaratorList.Declarators.Single().As<VarDeclNode>().Initializer, Is.InstanceOf<LambdaFuncExprNode>());
      }
        
        protected override ASTNode GenerateAST(string src)
            => new GoASTBuilder().BuildFromSource(src, p => p.statement().simpleStmt());
    }
}
