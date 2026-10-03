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
        /// <summary>
        /// Visits the import header parse tree context.
        /// </summary>
        /// <param name="ctx">The import header parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitImportHeader(KotlinParser.ImportHeaderContext ctx)
        {
            var path = ctx.identifier().GetText();
            return new ImportNode(ctx.Start.Line, path);
        }

        // Grammar rule: propertyDeclaration : modifierList? (VAL | VAR) ... (multiVariableDeclaration | variableDeclaration) ...
        /// <summary>
        /// Visits the property declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The property declaration parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPropertyDeclaration(KotlinParser.PropertyDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            string keyword = ctx.VAL() != null ? "val" : "var";
            string name = ctx.variableDeclaration().simpleIdentifier().GetText();
            string? typeName = ctx.variableDeclaration().type()?.GetText();
            ExprNode? init = ctx.expression() != null ? this.AsExprNode(this.Visit(ctx.expression())) : null;

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
        /// <summary>
        /// Visits the variable declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The variable declaration parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitVariableDeclaration(KotlinParser.VariableDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            IdNode idNode = new IdNode(line, ctx.simpleIdentifier().GetText());
            return new VarDeclNode(line, idNode);
        }


        // Grammar rule: typeAlias : modifierList? TYPE_ALIAS NL* simpleIdentifier ASSIGNMENT NL* type
        /// <summary>
        /// Visits the type alias parse tree context.
        /// </summary>
        /// <param name="ctx">The type alias parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeAlias(KotlinParser.TypeAliasContext ctx)
        {
            int line = ctx.Start.Line;
            IdNode aliasId = new IdNode(line, ctx.simpleIdentifier().GetText());
            IdNode typeId = new IdNode(line, ctx.type().GetText());
            DeclSpecsNode declSpecs = new DeclSpecsNode(line, "__linvast_typealias", ctx.type().GetText());
            VarDeclNode varDecl = new VarDeclNode(line, aliasId, typeId);
            DeclListNode declList = new DeclListNode(line, varDecl);
            return new DeclStatNode(line, declSpecs, declList);
        }

        // Grammar rule: classDeclaration : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier (NL* typeParameters)? 
        //                                  (NL* primaryConstructor)? (NL* COLON NL* delegationSpecifiers)? (NL* typeConstraints)? 
        //                                  (NL* classBody | NL* enumClassBody)?
        /// <summary>
        /// Visits the class declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The class declaration parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
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

        // Grammar rule: objectDeclaration : modifierList? OBJECT NL* simpleIdentifier (NL* primaryConstructor)? 
        //                                    (NL* COLON NL* delegationSpecifiers)? (NL* classBody)?
        /// <summary>
        /// Visits the object declaration parse tree context.
        /// </summary>        /// <param name="ctx">The object declaration parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitObjectDeclaration(KotlinParser.ObjectDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            var identifier = new IdNode(line, ctx.simpleIdentifier().GetText());

            TypeNameListNode baseTypes = ctx.delegationSpecifiers() != null
                ? this.Visit(ctx.delegationSpecifiers()).As<TypeNameListNode>()
                : new TypeNameListNode(line);

            var declarations = new List<DeclStatNode>();

            if (ctx.primaryConstructor() != null)
            {
                FuncParamsNode ctorParams = this.Visit(ctx.primaryConstructor()).As<FuncParamsNode>();
                FuncDeclNode ctorDecl = new FuncDeclNode(line, identifier, ctorParams);
                DeclSpecsNode ctorSpecs = new DeclSpecsNode(line, identifier.Identifier);
                declarations.Add(new DeclStatNode(line, ctorSpecs, new DeclListNode(line, ctorDecl)));
            }

            if (ctx.classBody() != null)
                declarations.AddRange(this.Visit(ctx.classBody()).As<BlockStatNode>().Children.Cast<DeclStatNode>());

            var declSpecs = new DeclSpecsNode(line, "object", identifier.Identifier);
            var typeDecl = new TypeDeclNode(line, identifier, new TypeNameListNode(line), baseTypes, declarations);
            return new ClassNode(line, declSpecs, typeDecl);
        }

        // Grammar rule: primaryConstructor : modifierList? (CONSTRUCTOR NL*)? classParameters
        /// <summary>
        /// Visits the primary constructor parse tree context.
        /// </summary>
        /// <param name="ctx">The primary constructor parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPrimaryConstructor(KotlinParser.PrimaryConstructorContext ctx)
            => this.Visit(ctx.classParameters());

        // Grammar rule: classParameters : LPAREN (classParameter (COMMA classParameter)* COMMA?)? RPAREN
        /// <summary>
        /// Visits the class parameters parse tree context.
        /// </summary>
        /// <param name="ctx">The class parameters parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitClassParameters(KotlinParser.ClassParametersContext ctx) 
        {
            int line = ctx.Start.Line;
            FuncParamNode[] paramNodes = ctx.classParameter().Select(p => this.Visit(p).As<FuncParamNode>()).ToArray();
            return new FuncParamsNode(line, paramNodes);
        }

        // Grammar rule: classParameter : modifierList? (VAL | VAR)? simpleIdentifier COLON type (ASSIGNMENT expression)?
        /// <summary>
        /// Visits the class parameter parse tree context.
        /// </summary>
        /// <param name="ctx">The class parameter parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
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
        /// <summary>
        /// Visits the delegation specifiers parse tree context.
        /// </summary>
        /// <param name="ctx">The delegation specifiers parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitDelegationSpecifiers(KotlinParser.DelegationSpecifiersContext ctx) 
        {
            int line = ctx.Start.Line;
            TypeNameNode[] types = ctx.delegationSpecifier().Select(p => this.Visit(p).As<TypeNameNode>()).ToArray();
            return new TypeNameListNode(line, types);
        }

        // Grammar rule: delegationSpecifier : constructorInvocation | userType | explicitDelegation
        /// <summary>
        /// Visits the delegation specifier parse tree context.
        /// </summary>
        /// <param name="ctx">The delegation specifier parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitDelegationSpecifier(KotlinParser.DelegationSpecifierContext ctx)
        {
            if(ctx.explicitDelegation() != null) return this.Visit(ctx.explicitDelegation().userType());
            if(ctx.constructorInvocation() != null) return this.Visit(ctx.constructorInvocation().userType());
            return this.Visit(ctx.userType());
        }

        // Grammar rule: classBody : LCURL classMemberDeclaration* RCURL
        /// <summary>
        /// Visits the class body parse tree context.
        /// </summary>
        /// <param name="ctx">The class body parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitClassBody(KotlinParser.ClassBodyContext ctx) 
        {
            int line = ctx.Start.Line;
            var members = ctx.classMemberDeclaration().Select(m => this.Visit(m).As<DeclStatNode>()).ToArray();
            return new BlockStatNode(line, members);
        }

        // Grammar rule: classMemberDeclaration : classDeclaration | functionDeclaration | objectDeclaration | companionObject | 
        //                                        propertyDeclaration | anonymousInitializer | secondaryConstructor | typeAlias
        /// <summary>
        /// Visits the class member declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The class member declaration parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitClassMemberDeclaration(KotlinParser.ClassMemberDeclarationContext ctx)
        {
            if (ctx.functionDeclaration() != null) return this.Visit(ctx.functionDeclaration());
            if (ctx.propertyDeclaration() != null) return this.Visit(ctx.propertyDeclaration());
            if (ctx.classDeclaration() != null) return this.Visit(ctx.classDeclaration());
            if (ctx.typeAlias() != null) return this.Visit(ctx.typeAlias());
            if (ctx.objectDeclaration() != null) return this.Visit(ctx.objectDeclaration());
            if (ctx.companionObject() != null) return this.Visit(ctx.companionObject());
            if (ctx.anonymousInitializer() != null) return this.Visit(ctx.anonymousInitializer());
            if (ctx.secondaryConstructor() != null) return this.Visit(ctx.secondaryConstructor());
            throw new NotImplementedException("unsupported class member");
        }

        // Grammar rule: companionObject : modifierList? COMPANION NL* OBJECT (NL* simpleIdentifier)? (NL* COLON NL* delegationSpecifiers)? (NL* classBody)?
        /// <summary>
        /// Visits the companion object parse tree context.
        /// </summary>
        /// <param name="ctx">The companion object parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitCompanionObject(KotlinParser.CompanionObjectContext ctx)
        {
            int line = ctx.Start.Line;
            string name = ctx.simpleIdentifier() != null ? ctx.simpleIdentifier().GetText() : "Companion";
            IdNode id = new IdNode(line, name);
            DeclSpecsNode specs = new DeclSpecsNode(line, "companion object", name);
            var declarations = new List<DeclStatNode>();
            if (ctx.classBody() != null)
                declarations.AddRange(this.Visit(ctx.classBody()).As<BlockStatNode>().Children.Cast<DeclStatNode>());
            TypeDeclNode typeDecl = new TypeDeclNode(line, id, new TypeNameListNode(line), new TypeNameListNode(line), declarations);
            return new DeclStatNode(line, specs, new DeclListNode(line, typeDecl));
        }

        // Grammar rule: anonymousInitializer : INIT NL* block
        /// <summary>
        /// Visits the anonymous initializer parse tree context.
        /// </summary>
        /// <param name="ctx">The anonymous initializer parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitAnonymousInitializer(KotlinParser.AnonymousInitializerContext ctx)
        {
            int line = ctx.Start.Line;
            BlockStatNode body = this.Visit(ctx.block()).As<BlockStatNode>();
            DeclSpecsNode specs = new DeclSpecsNode(line, "__linvast_init", "init");
            VarDeclNode initDecl = new VarDeclNode(line, new IdNode(line, "__init__"));
            return new DeclStatNode(line, specs, new DeclListNode(line, initDecl));
        }

        // Grammar rule: secondaryConstructor : modifierList? CONSTRUCTOR NL* functionValueParameters (NL* COLON NL* constructorDelegationCall)? NL* block
        /// <summary>
        /// Visits the secondary constructor parse tree context.
        /// </summary>
        /// <param name="ctx">The secondary constructor parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSecondaryConstructor(KotlinParser.SecondaryConstructorContext ctx)
        {
            int line = ctx.Start.Line;
            FuncParamsNode ctorParams = this.Visit(ctx.functionValueParameters()).As<FuncParamsNode>();
            BlockStatNode body = ctx.block() != null
                ? this.Visit(ctx.block()).As<BlockStatNode>()
                : new BlockStatNode(line);
            FuncDeclNode ctorDecl = new FuncDeclNode(line, new IdNode(line, "__init__"), ctorParams, body);
            DeclSpecsNode ctorSpecs = new DeclSpecsNode(line, "constructor");
            return new DeclStatNode(line, ctorSpecs, new DeclListNode(line, ctorDecl));
        }
    }
}
