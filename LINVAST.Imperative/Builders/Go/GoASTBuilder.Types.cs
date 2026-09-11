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
using System.Collections.Generic;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using System.Linq;


namespace LINVAST.Imperative.Builders.Go
{
    /// <summary>
    /// Builds a Go language AST from source code.
    /// </summary>

    public sealed partial class GoASTBuilder : GoParserBaseVisitor<ASTNode>, IASTBuilder<GoParser>
    {
        /// <summary>
        /// Visits the type_ parse tree context.
        /// </summary>
        /// <param name="context">The type_ parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitType_(GoParser.Type_Context context)
        {
            if (context.typeName() is not null) {
                return this.Visit(context.typeName()).As<TypeNameNode>();
            }
            if (context.typeLit() is not null) {
                return this.Visit(context.typeLit()).As<TypeNameNode>();
            }

            return this.Visit(context.type_()).As<TypeNameNode>();
        }

        /// <summary>
        /// Visits the type name parse tree context.
        /// </summary>
        /// <param name="context">The type name parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeName(GoParser.TypeNameContext context)
        {
            if (context.qualifiedIdent() is not null) {
                return this.Visit(context.qualifiedIdent()).As<TypeNameNode>();
            }
            string name = context.IDENTIFIER().GetText();
            return new TypeNameNode(context.Start.Line, name);
        }

        /// <summary>
        /// Visits the type lit parse tree context.
        /// </summary>
        /// <param name="context">The type lit parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeLit(GoParser.TypeLitContext context) => this.Visit(context.children.Single());

        /// <summary>
        /// Visits the type list parse tree context.
        /// </summary>
        /// <param name="context">The type list parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeList(GoParser.TypeListContext context)
        {
            if (context.type_() is not null) {
                return new TypeNameListNode(context.Start.Line, context.type_().Select(t => this.Visit(t).As<TypeNameNode>()));
            };

            return new TypeNameListNode(context.Start.Line);
        }

        /// <summary>
        /// Visits the non named type parse tree context.
        /// </summary>
        /// <param name="context">The non named type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitNonNamedType(GoParser.NonNamedTypeContext context)
        {
            if (context.typeLit() is not null) {
                return this.Visit(context.typeLit()).As<TypeNameNode>();
            }

            return this.Visit(context.nonNamedType()).As<TypeNameNode>();
        }

        /// <summary>
        /// Visits the array length parse tree context.
        /// </summary>
        /// <param name="context">The array length parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitArrayLength(GoParser.ArrayLengthContext context) => this.Visit(context.expression()).As<ExprNode>();
        
        /// <summary>
        /// Visits the result parse tree context.
        /// </summary>
        /// <param name="context">The result parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitResult(GoParser.ResultContext context)
        {
            if (context.parameters() is not null) {
                return this.Visit(context.parameters());
            }
            return this.Visit(context.type_());
        }
        
        /// <summary>
        /// Visits the slice type parse tree context.
        /// </summary>
        /// <param name="context">The slice type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSliceType(GoParser.SliceTypeContext context)
            => new TypeNameNode(context.Start.Line, $"[]{this.Visit(context.elementType()).As<TypeNameNode>().GetText()}");

        /// <summary>
        /// Visits the function type parse tree context.
        /// </summary>
        /// <param name="context">The function type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionType(GoParser.FunctionTypeContext context) => this.Visit(context.signature());

        /// <summary>
        /// Visits the interface type parse tree context.
        /// </summary>
        /// <param name="context">The interface type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitInterfaceType(GoParser.InterfaceTypeContext context)
            => new TypeNameNode(context.Start.Line, context.GetText());
        
        /// <summary>
        /// Visits the array type parse tree context.
        /// </summary>
        /// <param name="context">The array type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitArrayType(GoParser.ArrayTypeContext context)
            => new TypeNameNode(
                context.Start.Line,
                $"[{this.Visit(context.arrayLength()).As<ExprNode>().GetText()}]{this.Visit(context.elementType()).As<TypeNameNode>().GetText()}");
        
        /// <summary>
        /// Visits the struct type parse tree context.
        /// </summary>
        /// <param name="context">The struct type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitStructType(GoParser.StructTypeContext context)
            => new TypeNameNode(context.Start.Line, context.GetText());

        /// <summary>
        /// Visits the pointer type parse tree context.
        /// </summary>
        /// <param name="context">The pointer type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPointerType(GoParser.PointerTypeContext context)
            => new TypeNameNode(context.Start.Line, $"*{this.Visit(context.type_()).As<TypeNameNode>().GetText()}");

        /// <summary>
        /// Visits the method spec parse tree context.
        /// </summary>
        /// <param name="context">The method spec parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitMethodSpec(GoParser.MethodSpecContext context)
        {
            var identifier = new IdNode(context.Start.Line, context.IDENTIFIER().GetText());
            FuncParamsNode parameters = this.Visit(context.parameters()).As<FuncParamsNode>();
            TypeNameNode returnType = context.result() is null
                ? new TypeNameNode(context.Start.Line, "void")
                : ResultTypeName(context.result());
            var declSpecs = new DeclSpecsNode(context.Start.Line, returnType);
            return new DeclStatNode(context.Start.Line, declSpecs, new DeclListNode(context.Start.Line, new FuncDeclNode(context.Start.Line, identifier, parameters)));
        }

        /// <summary>
        /// Visits the map type parse tree context.
        /// </summary>
        /// <param name="context">The map type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitMapType(GoParser.MapTypeContext context)
            => new TypeNameNode(
                context.Start.Line,
                $"map[{this.Visit(context.type_()).As<TypeNameNode>().GetText()}]{this.Visit(context.elementType()).As<TypeNameNode>().GetText()}");
      
        /// <summary>
        /// Visits the channel type parse tree context.
        /// </summary>
        /// <param name="context">The channel type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitChannelType(GoParser.ChannelTypeContext context)
            => new TypeNameNode(context.Start.Line, context.GetText());

        /// <summary>
        /// Visits the conversion parse tree context.
        /// </summary>
        /// <param name="context">The conversion parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitConversion(GoParser.ConversionContext context)
        {
            TypeNameNode type = this.Visit(context.nonNamedType()).As<TypeNameNode>();
            ExprNode expression = this.Visit(context.expression()).As<ExprNode>();
            return new ConsExprNode(context.Start.Line, new IdNode(context.Start.Line, type.GetText()), new ExprListNode(context.Start.Line, expression));
        }
        
        /// <summary>
        /// Visits the embedded field parse tree context.
        /// </summary>
        /// <param name="context">The embedded field parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitEmbeddedField(GoParser.EmbeddedFieldContext context)
            => new TypeNameNode(context.Start.Line, context.GetText());

        /// <summary>
        /// Visits the field decl parse tree context.
        /// </summary>
        /// <param name="context">The field decl parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFieldDecl(GoParser.FieldDeclContext context)
        {
            TypeNameNode type = context.type_() is not null
                ? this.Visit(context.type_()).As<TypeNameNode>()
                : this.Visit(context.embeddedField()).As<TypeNameNode>();
            var declSpecs = new DeclSpecsNode(context.Start.Line, type);
            IEnumerable<DeclNode> declarators = context.identifierList() is null
                ? new[] { new VarDeclNode(context.Start.Line, new IdNode(context.Start.Line, type.GetText())) }
                : this.Visit(context.identifierList()).As<IdListNode>().Identifiers.Select(id => new VarDeclNode(id.Line, id));
            return new DeclStatNode(context.Start.Line, declSpecs, new DeclListNode(context.Start.Line, declarators));
        }

        private TypeNameNode ResultTypeName(GoParser.ResultContext context)
        {
            if (context.type_() is not null)
                return this.Visit(context.type_()).As<TypeNameNode>();

            FuncParamsNode parameters = this.Visit(context.parameters()).As<FuncParamsNode>();
            return new TypeNameNode(context.Start.Line, $"({string.Join(", ", parameters.Parameters.Select(ResultParameterText))})");
        }

        private static string ResultParameterText(FuncParamNode parameter)
        {
            string identifier = parameter.Declarator.Identifier;
            return identifier == "."
                ? parameter.Specifiers.TypeName
                : $"{parameter.Specifiers.TypeName} {identifier}";
        }
    }
}
