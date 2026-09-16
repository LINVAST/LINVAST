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
using System.Linq;
using Antlr4.Runtime.Misc;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.Java.JavaParser;

namespace LINVAST.Imperative.Builders.Java
{
    /// <summary>
    /// Builds a Java language AST from source code.
    /// </summary>

    public sealed partial class JavaASTBuilder : JavaBaseVisitor<ASTNode>, IASTBuilder<JavaParser>
    {
        /// <summary>
        /// Visits the type declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The type declaration parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeDeclaration([NotNull] TypeDeclarationContext ctx)
        {
            int ctxStartLine = ctx.Start.Line;
            string? modifiers = "";
            if (ctx.classOrInterfaceModifier().Any()) {
                ctxStartLine = ctx.classOrInterfaceModifier().First().Start.Line;
                modifiers = string.Join(" ", ctx.classOrInterfaceModifier()
                    .Select(c => this.ProcessClassOrInterfaceModifier(c))
                    .Where(mod => !string.IsNullOrWhiteSpace(mod)));
            }

            if (ctx.annotationTypeDeclaration() is not null) {
                TypeDeclNode annotationDecl = this.Visit(ctx.annotationTypeDeclaration()).As<TypeDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new InterfaceNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, annotationDecl.Identifier), annotationDecl);
            }

            if (ctx.classDeclaration() is not null) {
                TypeDeclNode? classDecl = this.Visit(ctx.classDeclaration()).As<TypeDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new ClassNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, classDecl.Identifier), classDecl);
            }

            if (ctx.enumDeclaration() is not null) {
                EnumDeclNode? enumDecl = this.Visit(ctx.enumDeclaration()).As<EnumDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new EnumNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, enumDecl.Identifier), enumDecl);
            }

            if (ctx.interfaceDeclaration() is not null) {
                TypeDeclNode? interfaceDecl = this.Visit(ctx.interfaceDeclaration()).As<TypeDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new InterfaceNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, interfaceDecl.Identifier), interfaceDecl);
            }

            return new EmptyStatNode(ctx.Start.Line);
        }

        /// <summary>
        /// Visits the class or interface modifier parse tree context.
        /// </summary>
        /// <param name="ctx">The class or interface modifier parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitClassOrInterfaceModifier([NotNull] ClassOrInterfaceModifierContext ctx)
        {
            if (ctx.annotation() is not null)
                return this.Visit(ctx.annotation());

            return new DeclSpecsNode(ctx.Start.Line, ctx.children.First().GetText());
        }

        /// <summary>
        /// Visits the type type parse tree context.
        /// </summary>
        /// <param name="ctx">The type type parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeType([NotNull] TypeTypeContext ctx)
        {
            TypeNameNode type = ctx.primitiveType() is not null
                ? this.Visit(ctx.primitiveType()).As<TypeNameNode>()
                : this.Visit(ctx.classOrInterfaceType()).As<TypeNameNode>();

            return ctx.LBRACK().Any()
                ? new TypeNameNode(type.Line, $"{type.TypeName}{string.Concat(Enumerable.Repeat("[]", ctx.LBRACK().Length))}", type.TemplateArguments)
                : type;
        }


        /// <summary>
        /// Visits the type list parse tree context.
        /// </summary>
        /// <param name="ctx">The type list parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeList([NotNull] TypeListContext ctx)
        {
            IEnumerable<TypeNameNode>? typeNameNodes = ctx.typeType().Select(c => this.Visit(c).As<TypeNameNode>());
            return new TypeNameListNode(ctx.Start.Line, typeNameNodes);
        }

        /// <summary>
        /// Visits the type parameters parse tree context.
        /// </summary>
        /// <param name="ctx">The type parameters parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeParameters([NotNull] TypeParametersContext ctx)
        {
            IEnumerable<TypeNameNode>? typeNameNodes = ctx.typeParameter().Select(c => this.Visit(c).As<TypeNameNode>());
            return new TypeNameListNode(ctx.Start.Line, typeNameNodes);
        }

        /// <summary>
        /// Visits the type parameter parse tree context.
        /// </summary>
        /// <param name="ctx">The type parameter parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeParameter([NotNull] TypeParameterContext ctx)
        {
            TypeNameListNode baseList = ctx.typeBound() is not null ? this.Visit(ctx.typeBound()).As<TypeNameListNode>() : new TypeNameListNode(ctx.Start.Line);
            return new TypeNameNode(ctx.Start.Line, ctx.IDENTIFIER().GetText(), baseList.Types);
        }

        /// <summary>
        /// Visits the type bound parse tree context.
        /// </summary>
        /// <param name="ctx">The type bound parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeBound([NotNull] TypeBoundContext ctx)
        {
            IEnumerable<TypeNameNode>? typeNameNodes = ctx.typeType().Select(c => this.Visit(c).As<TypeNameNode>());
            return new TypeNameListNode(ctx.Start.Line, typeNameNodes);
        }


        /// <summary>
        /// Visits the primitive type parse tree context.
        /// </summary>
        /// <param name="ctx">The primitive type parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPrimitiveType([NotNull] PrimitiveTypeContext ctx)
            => new TypeNameNode(ctx.Start.Line, ctx.children.First().GetText());

        /// <summary>
        /// Visits the class type parse tree context.
        /// </summary>
        /// <param name="ctx">The class type parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitClassType([NotNull] ClassTypeContext ctx)
        {
            int ctxStartLine = ctx.Start.Line;

            TypeNameListNode templlist = new(ctxStartLine), baselist = new(ctxStartLine);
            if (ctx.classOrInterfaceType() is not null) {
                TypeNameNode typeName = this.Visit(ctx.classOrInterfaceType()).As<TypeNameNode>();

                ctxStartLine = ctx.classOrInterfaceType().Start.Line;

                baselist = new TypeNameListNode(ctxStartLine, typeName);
            }

            if (ctx.typeArguments() is not null)
                templlist = this.Visit(ctx.typeArguments()).As<TypeNameListNode>();

            var identifier = new IdNode(ctxStartLine, ctx.IDENTIFIER().GetText());
            return new TypeDeclNode(ctxStartLine, identifier, templlist, baselist, new ArrayList<DeclStatNode>());
        }

        /// <summary>
        /// Visits the class or interface type parse tree context.
        /// </summary>
        /// <param name="ctx">The class or interface type parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitClassOrInterfaceType([NotNull] ClassOrInterfaceTypeContext ctx)
        {
            var typeNames = new TypeNameListNode(ctx.Start.Line);
            if (ctx.typeArguments()?.Any() ?? false)
                typeNames = this.Visit(ctx.typeArguments().First()).As<TypeNameListNode>();
            
            string identifier = string.Join(".", ctx.IDENTIFIER().Select(id => id.GetText()));
            return new TypeNameNode(ctx.Start.Line, identifier, typeNames.Types);
        }
        /// <summary>
        /// Visits the type type or void parse tree context.
        /// </summary>
        /// <param name="ctx">The type type or void parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeTypeOrVoid([NotNull] TypeTypeOrVoidContext ctx)
        {
            if (ctx.typeType() is not null)
                return this.Visit(ctx.typeType());

            return new TypeNameNode(ctx.Start.Line, ctx.children.First().GetText());
        }

        /// <summary>
        /// Visits the non wildcard type arguments parse tree context.
        /// </summary>
        /// <param name="ctx">The non wildcard type arguments parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitNonWildcardTypeArguments([NotNull] NonWildcardTypeArgumentsContext ctx)
            => this.Visit(ctx.typeList());

        /// <summary>
        /// Visits the non wildcard type arguments or diamond parse tree context.
        /// </summary>
        /// <param name="ctx">The non wildcard type arguments or diamond parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitNonWildcardTypeArgumentsOrDiamond([NotNull] NonWildcardTypeArgumentsOrDiamondContext ctx)
        {
            if (ctx.nonWildcardTypeArguments() is null)
                return new TypeNameListNode(ctx.Start.Line);

            return this.Visit(ctx.nonWildcardTypeArguments());
        }

        /// <summary>
        /// Visits the type argument parse tree context.
        /// </summary>
        /// <param name="ctx">The type argument parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeArgument([NotNull] TypeArgumentContext ctx)
        {
            if (ctx.typeType() is null)
                return new TypeNameNode(ctx.Start.Line, "?");

            if (ctx.EXTENDS() is not null || ctx.SUPER() is not null) {
                //TODO EXTENDS/SUPER
                return this.Visit(ctx.typeType());
            }

            return this.Visit(ctx.typeType());
        }

        /// <summary>
        /// Visits the type arguments parse tree context.
        /// </summary>
        /// <param name="ctx">The type arguments parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeArguments([NotNull] TypeArgumentsContext ctx)
        {
            IEnumerable<TypeNameNode>? typeNameNodes = ctx.typeArgument().Select(c => this.Visit(c).As<TypeNameNode>());
            return new TypeNameListNode(ctx.Start.Line, typeNameNodes);
        }
    }
}
