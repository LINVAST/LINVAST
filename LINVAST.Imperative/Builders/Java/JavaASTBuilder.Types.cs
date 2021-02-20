﻿using System;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Logging;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.Java.JavaParser;

namespace LINVAST.Imperative.Builders.Java
{
    public sealed partial class JavaASTBuilder : JavaBaseVisitor<ASTNode>, IASTBuilder<JavaParser>
    {
        public override ASTNode VisitTypeDeclaration([NotNull] TypeDeclarationContext ctx)
        {

            var ctxStartLine = ctx.Start.Line;
            var modifiers = "";
            if (ctx.classOrInterfaceModifier() is { }) {
                ctxStartLine = ctx.classOrInterfaceModifier().First().Start.Line;

                modifiers = string.Join(" ", ctx.classOrInterfaceModifier().Select(c => c.GetText()));

            }

            if (ctx.annotationTypeDeclaration() is { }) {
                throw new NotImplementedException("annotations");
            }

            if (ctx.classDeclaration() is { }) {
                var classDecl = this.Visit(ctx.classDeclaration()).As<TypeDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new ClassNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, classDecl.Identifier), classDecl);
            }

            if (ctx.enumDeclaration() is { }) {
                var enumDecl = this.Visit(ctx.enumDeclaration()).As<EnumDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new EnumNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, enumDecl.Identifier), enumDecl);
            }

            if (ctx.interfaceDeclaration() is { }) {
                var interfaceDecl = this.Visit(ctx.interfaceDeclaration()).As<TypeDeclNode>();
                int declSpecsLine = ctxStartLine;
                return new InterfaceNode(ctx.Start.Line, new DeclSpecsNode(declSpecsLine, modifiers, interfaceDecl.Identifier), interfaceDecl);
            }
            return new EmptyStatNode(ctx.Start.Line);
        }

        public override ASTNode VisitClassOrInterfaceModifier([NotNull] ClassOrInterfaceModifierContext ctx)
        {

            if (ctx.annotation() is { })
                throw new NotImplementedException("annotations");

            return new DeclSpecsNode(ctx.Start.Line, ctx.children.First().GetText());

        }



        public override ASTNode VisitTypeType([NotNull] TypeTypeContext ctx)
        {

            if (ctx.annotation() is { } && ctx.annotation().Any()) {
                throw new NotImplementedException("annotations");
            }

            if (ctx.primitiveType() is { }) {
                return this.Visit(ctx.primitiveType());

            }

            return this.Visit(ctx.classOrInterfaceType());

        }


        public override ASTNode VisitTypeList([NotNull] TypeListContext ctx)
        {
            var typeNameNodes = ctx.typeType().Select(c => this.Visit(c).As<TypeNameNode>()).ToArray<TypeNameNode>();

            var templateNodes = typeNameNodes.Select(t => new TypeDeclNode(ctx.Start.Line, new IdNode(ctx.Start.Line, t.Identifier), new IdListNode(ctx.Start.Line, t.TemplateArguments.Select(ta => new IdNode(ctx.Start.Line, ta.Identifier)).ToArray<IdNode>()), new IdListNode(ctx.Start.Line, new ArrayList<IdNode>()), new ArrayList<DeclStatNode>()));

            return new TypeNameListNode(ctx.Start.Line, templateNodes);
        }

        public override ASTNode VisitTypeParameters([NotNull] TypeParametersContext ctx)
        {
            return new TypeNameListNode(ctx.Start.Line, ctx.typeParameter().Select(c => this.Visit(c).As<TypeDeclNode>()).ToArray());
        }

        public override ASTNode VisitTypeParameter([NotNull] TypeParameterContext ctx)
        {
            if (ctx.annotation() is { } && ctx.annotation().Any())
                throw new NotImplementedException("annotations");

            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            IdListNode baseList;
            if (ctx.typeBound() is { })
                baseList = new IdListNode(ctx.Start.Line, this.Visit(ctx.typeBound()).As<TypeNameListNode>().Types.Select(t => new IdNode(ctx.Start.Line, t.Identifier)).ToArray());
            else
                baseList = new IdListNode(ctx.Start.Line, new ArrayList<IdNode>());

            return new TypeDeclNode(ctx.Start.Line, identifier, new IdListNode(ctx.Start.Line, new ArrayList<IdNode>()), baseList, new ArrayList<DeclStatNode>());
        }

        public override ASTNode VisitTypeBound([NotNull] TypeBoundContext ctx)
        {
            var typeNameNodes = ctx.typeType().Select(c => this.Visit(c).As<TypeNameNode>()).ToArray<TypeNameNode>();

            var templateNodes = typeNameNodes.Select(t => new TypeDeclNode(ctx.Start.Line, new IdNode(ctx.Start.Line, t.Identifier), new IdListNode(ctx.Start.Line, t.TemplateArguments.Select(ta => new IdNode(ctx.Start.Line, ta.Identifier)).ToArray<IdNode>()), new IdListNode(ctx.Start.Line, new ArrayList<IdNode>()), new ArrayList<DeclStatNode>()));

            return new TypeNameListNode(ctx.Start.Line, templateNodes);
        }


        public override ASTNode VisitPrimitiveType([NotNull] PrimitiveTypeContext ctx)
        {

            return new TypeNameNode(ctx.Start.Line, ctx.children.First().GetText());

        }

        public override ASTNode VisitClassType([NotNull] ClassTypeContext ctx)
        {

            if (ctx.annotation() is { } && ctx.annotation().Any()) {
                throw new NotImplementedException("annotations");
            }

            var ctxStartLine = ctx.Start.Line;
            IdListNode templlist = new IdListNode(ctxStartLine, new ArrayList<IdNode>()), baselist = new IdListNode(ctxStartLine, new ArrayList<IdNode>());
            if (ctx.classOrInterfaceType() is { }) {
                TypeNameNode typeName = this.Visit(ctx.classOrInterfaceType()).As<TypeNameNode>();

                ctxStartLine = ctx.classOrInterfaceType().Start.Line;
                var list = new ArrayList<IdNode>();
                list.Add(new IdNode(ctxStartLine, typeName.Identifier));
                baselist = new IdListNode(ctxStartLine, list);
            }

            if (ctx.typeArguments() is { }) {
                
                templlist = new IdListNode(ctxStartLine, this.Visit(ctx.typeArguments()).As<TypeNameListNode>().Types.Select(t => new IdNode(ctxStartLine, t.Identifier)).ToArray<IdNode>());
            }


            var identifier = new IdNode(ctxStartLine, ctx.IDENTIFIER().GetText());
            return new TypeDeclNode(ctxStartLine, identifier, templlist, baselist, new ArrayList<DeclStatNode>());
        }

        public override ASTNode VisitClassOrInterfaceType([NotNull] ClassOrInterfaceTypeContext ctx)
        {

            if (ctx.IDENTIFIER().Length > 1)
                throw new NotImplementedException("base types");

            
            var typeArgumentsList = ctx.typeArguments().Any() ? this.Visit(ctx.typeArguments().First()).As<TypeNameListNode>().Types.Select(t => new IdNode(ctx.Start.Line, t.Identifier)).ToList<IdNode>() : new ArrayList<IdNode>();


            return new TypeNameNode(ctx.Start.Line, ctx.IDENTIFIER().First().GetText(), typeArgumentsList);

        }
        public override ASTNode VisitTypeTypeOrVoid([NotNull] TypeTypeOrVoidContext ctx)
        {
            if (ctx.typeType() is { })
                return this.Visit(ctx.typeType());

            return new TypeNameNode(ctx.Start.Line, ctx.children.First().GetText());
        }

        public override ASTNode VisitNonWildcardTypeArguments([NotNull] NonWildcardTypeArgumentsContext ctx)
        {
            return this.Visit(ctx.typeList());
        }

        public override ASTNode VisitNonWildcardTypeArgumentsOrDiamond([NotNull] NonWildcardTypeArgumentsOrDiamondContext ctx)
        {
            if (ctx.nonWildcardTypeArguments() is null)
                throw new NotImplementedException("<>");
               

                return this.Visit(ctx.nonWildcardTypeArguments());
        }

        public override ASTNode VisitTypeArgument([NotNull] TypeArgumentContext ctx)
        {

            if (ctx.annotation() is { } && ctx.annotation().Any())
                throw new NotImplementedException("annotations");

            if (ctx.EXTENDS() is { } || ctx.SUPER() is { }) {
                //TODO EXTENDS/SUPER
                return this.Visit(ctx.typeType());

            }
            return this.Visit(ctx.typeType());
        }

        public override ASTNode VisitTypeArguments([NotNull] TypeArgumentsContext ctx)
        {

            var typeNameNodes = ctx.typeArgument().Select(c => this.Visit(c).As<TypeNameNode>()).ToArray<TypeNameNode>();

            var templateNodes = typeNameNodes.Select(t => new TypeDeclNode(ctx.Start.Line, new IdNode(ctx.Start.Line, t.Identifier), new IdListNode(ctx.Start.Line, t.TemplateArguments.Select(ta => new IdNode(ctx.Start.Line, ta.Identifier)).ToArray<IdNode>()), new IdListNode(ctx.Start.Line, new ArrayList<IdNode>()), new ArrayList<DeclStatNode>()));

            return new TypeNameListNode(ctx.Start.Line, templateNodes);

        }

    }



}