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

﻿using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime.Misc;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.Pseudo.PseudoParser;

namespace LINVAST.Imperative.Builders.Pseudo
{
    /// <summary>
    /// Builds a Pseudo language AST from source code.
    /// </summary>

    public sealed partial class PseudoASTBuilder : PseudoBaseVisitor<ASTNode>, IASTBuilder<PseudoParser>
    {
        /// <summary>
        /// Visits the declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The declaration parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitDeclaration([NotNull] DeclarationContext ctx)
        {
            switch (ctx.children.First().GetText()) {
                case "declare":
                    var declSpecs = new DeclSpecsNode(ctx.Start.Line, GetTypeName());
                    var name = new IdNode(ctx.Start.Line, ctx.NAME().GetText());
                    DeclNode decl;
                    if (ctx.type().typename().children.Count > 1) {
                        switch (ctx.type().typename().children.Last().GetText()) {
                            case "array":
                            case "list":
                            case "set":
                                if (ctx.exp() is not null) {
                                    ExprNode init = this.Visit(ctx.exp()).As<ExprNode>();
                                    decl = new ArrDeclNode(ctx.Start.Line, name, init);
                                } else {
                                    decl = new ArrDeclNode(ctx.Start.Line, name);
                                }
                                break;
                            default:
                                throw new SyntaxErrorException("Invalid complex type");
                        }
                    } else {
                        if (ctx.exp() is not null) {
                            ExprNode init = this.Visit(ctx.exp()).As<ExprNode>();
                            decl = new VarDeclNode(ctx.Start.Line, name, init);
                        } else {
                            decl = new VarDeclNode(ctx.Start.Line, name);
                        }
                    }
                    var declList = new DeclListNode(ctx.Start.Line, decl);
                    return new DeclStatNode(ctx.Start.Line, declSpecs, declList);
                case "procedure":
                case "function":
                    var fdeclSpecs = new DeclSpecsNode(ctx.Start.Line, GetTypeName());
                    var fname = new IdNode(ctx.Start.Line, ctx.NAME().GetText());
                    FuncParamsNode? fparams = ctx.parlist() is null ? null : this.Visit(ctx.parlist()).As<FuncParamsNode>();
                    BlockStatNode body = this.Visit(ctx.block()).As<BlockStatNode>();
                    FuncDeclNode fdef = fparams is null
                        ? new FuncDeclNode(ctx.Start.Line, fname, body)
                        : new FuncDeclNode(ctx.Start.Line, fname, fparams, body);
                    return new FuncNode(ctx.Start.Line, fdeclSpecs, fdef);
                default:
                    throw new SyntaxErrorException("Invalid statement");
            }


            string GetTypeName() => ctx.type()?.typename().GetText() ?? "void";
        }

        /// <summary>
        /// Visits the parlist parse tree context.
        /// </summary>
        /// <param name="ctx">The parlist parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParlist([NotNull] ParlistContext ctx)
        {
            IEnumerable<FuncParamNode> @params = ctx.NAME().Zip(ctx.type(), (name, type) => {
                var declSpecs = new DeclSpecsNode(type.Start.Line, type.typename().GetText());
                var identifier = new IdNode(ctx.Start.Line, name.GetText());
                DeclNode decl;
                if (type.typename().children.Count > 1) {
                    switch (type.typename().children.Last().GetText()) {
                        case "array":
                        case "list":
                        case "set":
                            decl = new ArrDeclNode(ctx.Start.Line, identifier);
                            break;
                        default:
                            throw new SyntaxErrorException("Invalid complex type");
                    }
                } else {
                    decl = new VarDeclNode(ctx.Start.Line, identifier);
                }
                return new FuncParamNode(type.Start.Line, declSpecs, decl);
            });
            return new FuncParamsNode(ctx.Start.Line, @params);
        }
    }
}
