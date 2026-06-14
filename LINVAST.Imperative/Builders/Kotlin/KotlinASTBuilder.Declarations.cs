using System;
using System.Collections.Generic;
using System.Linq;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: importHeader : IMPORT identifier (DOT MULT | importAlias)? semi?
        public override ASTNode VisitImportHeader(KotlinParser.ImportHeaderContext ctx)
        {
            var path = ctx.identifier().GetText();
            return new ImportNode(ctx.Start.Line, path);
        }

        // Grammar rule: propertyDeclaration : modifierList? (VAL | VAR) ... (multiVariableDeclaration | variableDeclaration) ...
        public override ASTNode VisitPropertyDeclaration(KotlinParser.PropertyDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            string keyword = ctx.VAL() != null ? "val" : "var";
            string name = ctx.variableDeclaration().simpleIdentifier().GetText();
            string? typeName = ctx.variableDeclaration().type()?.GetText();
            ExprNode? init = ctx.expression() != null ? this.Visit(ctx.expression()).As<ExprNode>() : null;

            DeclSpecsNode declSpecs = typeName != null
                ? new DeclSpecsNode(line, keyword, typeName)
                : new DeclSpecsNode(line, keyword);

            IdNode id = new IdNode(line, name);
            VarDeclNode varDecl = init is not null
                ? new VarDeclNode(line, id, init)
                : new VarDeclNode(line, id);

            DeclListNode declList = new DeclListNode(line, varDecl);

            return new DeclStatNode(line, declSpecs, declList);
        }

        // Grammar rule: variableDeclaration : simpleIdentifier (COLON type)?
        public override ASTNode VisitVariableDeclaration(KotlinParser.VariableDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            IdNode idNode = new IdNode(line, ctx.simpleIdentifier().GetText());
            return new VarDeclNode(line, idNode);
        }


        // Grammar rule: typeAlias : modifierList? TYPE_ALIAS NL* simpleIdentifier ASSIGNMENT NL* type
        public override ASTNode VisitTypeAlias(KotlinParser.TypeAliasContext ctx)
        {
            throw new NotImplementedException("typeAlias is not supported");
        }

        // Grammar rule: classDeclaration : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier (NL* typeParameters)? 
        //                                  (NL* primaryConstructor)? (NL* COLON NL* delegationSpecifiers)? (NL* typeConstraints)? 
        //                                  (NL* classBody | NL* enumClassBody)?
        public override ASTNode VisitClassDeclaration(KotlinParser.ClassDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            var identifier = new IdNode(line, ctx.simpleIdentifier().GetText());

            TypeNameListNode typeParams = ctx.typeParameters() != null 
                                        ? this.Visit(ctx.typeParameters()).As<TypeNameListNode>()
                                        : new TypeNameListNode(line);
            
            TypeNameListNode baseTypes = ctx.delegationSpecifiers() != null
                                      ? this.Visit(ctx.delegationSpecifiers()).As<TypeNameListNode>()
                                      : new TypeNameListNode(line);

            if (ctx.typeConstraints() != null)
                throw new NotImplementedException("typeConstraints (where clause) is not supported");

            var declarations = new List<DeclStatNode>();

            if (ctx.primaryConstructor() != null) {
                FuncParamsNode ctorParams = this.Visit(ctx.primaryConstructor()).As<FuncParamsNode>();
                FuncDeclNode ctorDecl = new FuncDeclNode(line, identifier, ctorParams);
                DeclSpecsNode ctorSpecs = new DeclSpecsNode(line, identifier.Identifier);
                declarations.Add(new DeclStatNode(line, ctorSpecs, new DeclListNode(line, ctorDecl)));
            }

            if (ctx.classBody() != null)
                declarations.AddRange(this.Visit(ctx.classBody()).As<BlockStatNode>().Children.Cast<DeclStatNode>());

            var declSpecs = new DeclSpecsNode(line, identifier.Identifier);
            var typeDecl = new TypeDeclNode(line, identifier, typeParams, baseTypes, declarations);

            if (ctx.INTERFACE() != null) return new InterfaceNode(line, declSpecs, typeDecl);
            return new ClassNode(line, declSpecs, typeDecl);
        }

        // Grammar rule: primaryConstructor : modifierList? (CONSTRUCTOR NL*)? classParameters
        public override ASTNode VisitPrimaryConstructor(KotlinParser.PrimaryConstructorContext ctx)
            => this.Visit(ctx.classParameters());

        // Grammar rule: classParameters : LPAREN (classParameter (COMMA classParameter)* COMMA?)? RPAREN
        public override ASTNode VisitClassParameters(KotlinParser.ClassParametersContext ctx) 
        {
            int line = ctx.Start.Line;
            FuncParamNode[] paramNodes = ctx.classParameter().Select(p => this.Visit(p).As<FuncParamNode>()).ToArray();
            return new FuncParamsNode(line, paramNodes);
        }

        // Grammar rule: classParameter : modifierList? (VAL | VAR)? simpleIdentifier COLON type (ASSIGNMENT expression)?
        public override ASTNode VisitClassParameter(KotlinParser.ClassParameterContext ctx) 
        {
            int line = ctx.Start.Line;
            string modifier = ctx.VAL() != null ? "val" : ctx.VAR() != null ? "var" : "";
            IdNode id = new IdNode(line, ctx.simpleIdentifier().GetText());
            TypeNameNode type = this.Visit(ctx.type()).As<TypeNameNode>();
            DeclSpecsNode specs = string.IsNullOrEmpty(modifier) 
                                ? new DeclSpecsNode(line, type)
                                : new DeclSpecsNode(line, modifier, type);
            return new FuncParamNode(line, specs, new VarDeclNode(line, id));
        }

        // Grammar rule: delegationSpecifiers : delegationSpecifier (NL* COMMA NL* delegationSpecifier)*
        public override ASTNode VisitDelegationSpecifiers(KotlinParser.DelegationSpecifiersContext ctx) 
        {
            int line = ctx.Start.Line;
            TypeNameNode[] types = ctx.delegationSpecifier().Select(p => this.Visit(p).As<TypeNameNode>()).ToArray();
            return new TypeNameListNode(line, types);
        }

        // Grammar rule: delegationSpecifier : constructorInvocation | userType | explicitDelegation
        public override ASTNode VisitDelegationSpecifier(KotlinParser.DelegationSpecifierContext ctx) 
        {
            if(ctx.explicitDelegation() != null) throw new NotImplementedException("explicitDelegation is not supported");
            if(ctx.constructorInvocation() != null) return this.Visit(ctx.constructorInvocation().userType());
            return this.Visit(ctx.userType());
        }

        // Grammar rule: classBody : LCURL classMemberDeclaration* RCURL
        public override ASTNode VisitClassBody(KotlinParser.ClassBodyContext ctx) 
        {
            int line = ctx.Start.Line;
            var members = ctx.classMemberDeclaration().Select(m => this.Visit(m).As<DeclStatNode>()).ToArray();
            return new BlockStatNode(line, members);
        }

        // Grammar rule: classMemberDeclaration : classDeclaration | functionDeclaration | objectDeclaration | companionObject | 
        //                                        propertyDeclaration | anonymousInitializer | secondaryConstructor | typeAlias
        public override ASTNode VisitClassMemberDeclaration(KotlinParser.ClassMemberDeclarationContext ctx)
        {
            if (ctx.functionDeclaration() != null) return this.Visit(ctx.functionDeclaration());
            if (ctx.propertyDeclaration() != null) return this.Visit(ctx.propertyDeclaration());
            if (ctx.classDeclaration() != null) return this.Visit(ctx.classDeclaration());
            throw new NotImplementedException("unsupported class member");
        }
    }
}
