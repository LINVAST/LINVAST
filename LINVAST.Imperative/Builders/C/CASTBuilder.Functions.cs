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
using Antlr4.Runtime.Misc;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.C.CParser;

namespace LINVAST.Imperative.Builders.C
{
    /// <summary>
    /// Builds a C language AST from source code.
    /// </summary>

    public sealed partial class CASTBuilder : CBaseVisitor<ASTNode>, IASTBuilder<CParser>
    {
        /// <summary>
        /// Visits the function definition parse tree context.
        /// </summary>
        /// <param name="ctx">The function definition parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionDefinition([NotNull] FunctionDefinitionContext ctx)
        {
            DeclSpecsNode declSpecs = this.Visit(ctx.declarationSpecifiers()).As<DeclSpecsNode>();
            ASTNode decl = this.Visit(ctx.declarator());
            if (decl is IdNode fname)
                decl = new FuncDeclNode(fname.Line, fname);
            FuncDeclNode fdecl = decl.As<FuncDeclNode>();
            BlockStatNode body = this.Visit(ctx.compoundStatement()).As<BlockStatNode>();
            FuncDeclNode fdef = fdecl.ParametersNode is not null
                ? new FuncDeclNode(fdecl.Line, fdecl.IdentifierNode, fdecl.ParametersNode, body)
                : new FuncDeclNode(fdecl.Line, fdecl.IdentifierNode, body);
            return new FuncNode(ctx.Start.Line, declSpecs, fdef);
        }

        /// <summary>
        /// Visits the parameter type list parse tree context.
        /// </summary>
        /// <param name="ctx">The parameter type list parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParameterTypeList([NotNull] ParameterTypeListContext ctx)
        {
            FuncParamsNode @params = this.Visit(ctx.parameterList()).As<FuncParamsNode>();
            if (ctx.ChildCount > 1)
                @params.IsVariadic = true;
            return @params;
        }

        /// <summary>
        /// Visits the parameter list parse tree context.
        /// </summary>
        /// <param name="ctx">The parameter list parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParameterList([NotNull] ParameterListContext ctx)
        {
            FuncParamsNode @params;
            FuncParamNode param = this.Visit(ctx.parameterDeclaration()).As<FuncParamNode>();

            if (ctx.parameterList() is null)
                return new FuncParamsNode(ctx.Start.Line, param);

            @params = this.Visit(ctx.parameterList()).As<FuncParamsNode>();
            return new FuncParamsNode(ctx.Start.Line, @params.Parameters.Concat(new[] { param }));
        }

        /// <summary>
        /// Visits the parameter declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The parameter declaration parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParameterDeclaration([NotNull] ParameterDeclarationContext ctx)
        {
            DeclSpecsNode declSpecs = this.Visit(ctx.declarationSpecifiers()).As<DeclSpecsNode>();
            DeclNode decl = this.Visit(ctx.declarator()).As<DeclNode>();
            return new FuncParamNode(ctx.Start.Line, declSpecs, decl);
        }
    }
}
