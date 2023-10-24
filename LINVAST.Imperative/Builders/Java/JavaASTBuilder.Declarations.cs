using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Antlr4.Runtime.Misc;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.Java.JavaParser;

namespace LINVAST.Imperative.Builders.Java
{
    public sealed partial class JavaASTBuilder : JavaBaseVisitor<ASTNode>, IASTBuilder<JavaParser>
    {

        #region package and import declarations

        public override ASTNode VisitPackageDeclaration([NotNull] PackageDeclarationContext ctx)
            => new PackageNode(ctx.Start.Line, ctx.qualifiedName().GetText());

        public override ASTNode VisitImportDeclaration([NotNull] ImportDeclarationContext ctx)
        {
            if (ctx.STATIC() is { })
                throw new NotImplementedException("static import");

            var directive = new StringBuilder(this.Visit(ctx.qualifiedName()).As<IdNode>().GetText());
            if (ctx.MUL() is { })
                directive.Append(".*");

            return new ImportNode(ctx.Start.Line, directive.ToString());
        }

        #endregion

        #region class, enum, interface declarations

        public override ASTNode VisitClassDeclaration([NotNull] ClassDeclarationContext ctx)
        {
            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            TypeNameListNode templateParams = ctx.typeParameters() is { } typeParamsCtx
                ? this.Visit(typeParamsCtx).As<TypeNameListNode>()
                : new TypeNameListNode(ctx.Start.Line);
            IEnumerable<TypeNameNode> baseTypes;
            int baseTypesStartLine = ctx.Start.Line;
            if (ctx.typeList() is { } typeListCtx) {
                baseTypes = this.Visit(typeListCtx).As<TypeNameListNode>().Types;
                baseTypesStartLine = typeListCtx.Start.Line;
            } else {
                baseTypes = new TypeNameNode[] { };
            }

            if (ctx.typeType() is { } typeTypeCtx) {
                baseTypes = baseTypes.Append(this.Visit(typeTypeCtx).As<TypeNameNode>());
                baseTypesStartLine = typeTypeCtx.Start.Line;
            }

            IEnumerable<DeclStatNode> declarations;
            BlockStatNode block = this.Visit(ctx.classBody()).As<BlockStatNode>();
            declarations = block.Children.Cast<DeclStatNode>();

            return new TypeDeclNode(ctx.Start.Line, identifier, templateParams,
                new TypeNameListNode(baseTypesStartLine, baseTypes),
                declarations);
        }

        public override ASTNode VisitEnumDeclaration([NotNull] EnumDeclarationContext ctx)
        {
            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            if (ctx.typeList() is { })
                throw new NotImplementedException("enum implements interface");

            IEnumerable<VarDeclNode> constants = new VarDeclNode[] { };
            if (ctx.enumConstants() is { }) {
                BlockStatNode block = this.Visit(ctx.enumConstants()).As<BlockStatNode>();
                constants = block.Children.Select(c => c.As<VarDeclNode>());
            }

            if (ctx.enumBodyDeclarations() is { })
                throw new NotImplementedException("enum class body");

            return new EnumDeclNode(ctx.Start.Line, identifier, constants);
        }

        public override ASTNode VisitEnumBodyDeclarations([NotNull] EnumBodyDeclarationsContext ctx)
            => throw new NotImplementedException("Enum class body");

        public override ASTNode VisitInterfaceDeclaration([NotNull] InterfaceDeclarationContext ctx)
        {
            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            TypeNameListNode? templateParams = null;
            if (ctx.typeParameters() is { })
                templateParams = this.Visit(ctx.typeParameters()).As<TypeNameListNode>();

            TypeNameListNode? baseTypes = null;
            if (ctx.typeList() is { })
                baseTypes = this.Visit(ctx.typeList()).As<TypeNameListNode>();

            IEnumerable<DeclStatNode> declarations;
            BlockStatNode block = this.Visit(ctx.interfaceBody()).As<BlockStatNode>();
            declarations = block.Children.Select(c => c.As<DeclStatNode>());

            return new TypeDeclNode(ctx.Start.Line, identifier,
                templateParams ?? new TypeNameListNode(ctx.Start.Line),
                baseTypes ?? new TypeNameListNode(ctx.Start.Line),
                declarations);
        }

        public override ASTNode VisitClassBodyDeclaration([NotNull] ClassBodyDeclarationContext ctx)
        {
            if (ctx.SEMI() is { })
                return new EmptyStatNode(ctx.Start.Line);

            if (ctx.STATIC() is { } || ctx.block() is { })
                throw new NotImplementedException("static- and non-static- blocks in a class");

            // we use private method ProcessModifier instead of overriding VisitModifier
            string modifiers = "";
            int? declSpecsStartLine = null;
            if (ctx.modifier() is { } modifierCtxList && modifierCtxList.Any()) {
                modifiers = string.Join(" ", modifierCtxList.Select(modCtx => this.ProcessModifier(modCtx)));
                declSpecsStartLine = modifierCtxList.First().Start.Line;
            }

            MemberDeclarationContext memberDeclCtx = ctx.memberDeclaration();

            DeclSpecsNode declSpecs;
            TypeNameNode? typeName = TypeName(memberDeclCtx);
            if (typeName is { }) { // if memberDeclaration is anything but constructor- or genericConstructor- Declaration
                declSpecsStartLine ??= typeName.Line;
                declSpecs = new DeclSpecsNode(declSpecsStartLine ?? ctx.Start.Line, modifiers, typeName);
            } else { // if memberDeclaration is constructor- or genericConstructor- Declaration
                throw new NotImplementedException("constructors");
            }
            DeclListNode declList = memberDeclCtx.fieldDeclaration() is { }
                ? this.Visit(memberDeclCtx).As<DeclListNode>()
                : new DeclListNode(memberDeclCtx.Start.Line, this.Visit(memberDeclCtx).As<DeclNode>());
            return new DeclStatNode(ctx.Start.Line, declSpecs, declList);


            TypeNameNode? TypeName([NotNull] MemberDeclarationContext ctx)
            {
                if (ctx.classDeclaration() is { } clsDeclCtx)
                    return new TypeNameNode(clsDeclCtx.Start.Line, clsDeclCtx.IDENTIFIER().GetText());

                if (ctx.interfaceDeclaration() is { } interfaceDeclCtx)
                    return new TypeNameNode(interfaceDeclCtx.Start.Line, interfaceDeclCtx.IDENTIFIER().GetText());

                if (ctx.enumDeclaration() is { } enumDeclCtx)
                    return new TypeNameNode(enumDeclCtx.Start.Line, enumDeclCtx.IDENTIFIER().GetText());

                if (ctx.annotationTypeDeclaration() is { })
                    throw new NotImplementedException("annotation type declaration");

                if (ctx.methodDeclaration() is { } methodDeclCtx)
                    return this.Visit(methodDeclCtx.typeTypeOrVoid()).As<TypeNameNode>();

                if (ctx.genericMethodDeclaration() is { } genMethDeclCtx)
                    return this.Visit(genMethDeclCtx.methodDeclaration().typeTypeOrVoid()).As<TypeNameNode>();

                if (ctx.fieldDeclaration() is { } fieldDeclCtx)
                    return this.Visit(fieldDeclCtx.typeType()).As<TypeNameNode>();

                if (ctx.constructorDeclaration() is { })
                    return null;

                if (ctx.genericConstructorDeclaration() is { })
                    return null;

                // unreachable path
                throw new SyntaxErrorException("Source file contained unexpected content");
            }
        }

        #endregion

        #region class member declarations

        public override ASTNode VisitMemberDeclaration([NotNull] MemberDeclarationContext ctx)
            => this.Visit(ctx.children.Single());

        public override ASTNode VisitMethodDeclaration([NotNull] MethodDeclarationContext ctx)
        {
            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            FuncParamsNode @params = this.Visit(ctx.formalParameters()).As<FuncParamsNode>();

            // brackets applies to the return type, historical reasons
            if (ctx.LBRACK().Length > 0)
                throw new NotImplementedException("brackets after method definition");

            if (ctx.THROWS() is { })
                throw new NotImplementedException("exceptions");

            BlockStatNode body = this.Visit(ctx.methodBody()).As<BlockStatNode>();

            return new FuncDeclNode(ctx.Start.Line, identifier, @params, body);
        }

        public override ASTNode VisitGenericMethodDeclaration([NotNull] GenericMethodDeclarationContext ctx)
        {
            TypeNameListNode templateArgs = this.Visit(ctx.typeParameters()).As<TypeNameListNode>();
            FuncDeclNode func = this.Visit(ctx.methodDeclaration()).As<FuncDeclNode>();

            // throwing exception to suppress warnings
            return new FuncDeclNode(ctx.Start.Line, func.IdentifierNode, templateArgs,
                    func.ParametersNode ?? throw new SyntaxErrorException("Unknown construct"),
                    func.Definition ?? throw new SyntaxErrorException("Unknown construct"));
        }

        public override ASTNode VisitGenericConstructorDeclaration([NotNull] GenericConstructorDeclarationContext ctx)
            => throw new NotImplementedException("constructors");

        public override ASTNode VisitConstructorDeclaration([NotNull] ConstructorDeclarationContext ctx)
            => throw new NotImplementedException("constructors");

        public override ASTNode VisitFieldDeclaration([NotNull] FieldDeclarationContext ctx)
            => this.Visit(ctx.variableDeclarators()); // DeclListNode

        #endregion

        #region interface member declarations

        public override ASTNode VisitInterfaceBodyDeclaration([NotNull] InterfaceBodyDeclarationContext ctx)
        {
            if (ctx.SEMI() is { })
                return new EmptyStatNode(ctx.Start.Line);

            var modifiers = new StringBuilder("");
            int? declSpecsStartLine = null;
            if (ctx.modifier() is { } modifierCtxList && modifierCtxList.Any()) {
                modifiers.Append(string.Join(" ",
                    modifierCtxList.Select(modCtx => this.ProcessModifier(modCtx))));
                declSpecsStartLine = modifierCtxList.First().Start.Line;
            }

            // additionally, there could be interface specific method modifiers
            if (ctx.interfaceMemberDeclaration().interfaceMethodDeclaration() is { } iMethodDeclCtx &&
                iMethodDeclCtx.interfaceMethodModifier() is { } iMethodModCtxList &&
                iMethodModCtxList.Any()) {
                if (!modifiers.Equals(""))
                    modifiers.Append(" ");
                modifiers.Append(string.Join(" ",
                    iMethodModCtxList.Select(iMCtx => this.ProcessInterfaceMethodModifier(iMCtx))));
                declSpecsStartLine ??= iMethodModCtxList.First().Start.Line;
            }

            TypeNameNode type = TypeName(ctx.interfaceMemberDeclaration());
            declSpecsStartLine ??= type.Line;
            var declSpecs = new DeclSpecsNode(declSpecsStartLine ?? ctx.Start.Line, modifiers.ToString(), type);

            DeclListNode declList = ctx.interfaceMemberDeclaration().constDeclaration() is { } constDeclCtx
                ? this.Visit(constDeclCtx).As<DeclListNode>()
                : new DeclListNode(ctx.interfaceMemberDeclaration().Start.Line,
                    this.Visit(ctx.interfaceMemberDeclaration()).As<DeclNode>());
            return new DeclStatNode(ctx.Start.Line, declSpecs, declList);


            TypeNameNode TypeName([NotNull] InterfaceMemberDeclarationContext ctx)
            {
                if (ctx.constDeclaration() is { } constDeclCtx)
                    return this.Visit(constDeclCtx.typeType()).As<TypeNameNode>();

                if (ctx.interfaceMethodDeclaration() is { } iMethodDeclCtx)
                    return this.Visit(iMethodDeclCtx.typeTypeOrVoid()).As<TypeNameNode>();

                if (ctx.genericInterfaceMethodDeclaration() is { } genIntMethdDeclCtx)
                    return this.Visit(genIntMethdDeclCtx.interfaceMethodDeclaration().typeTypeOrVoid()).As<TypeNameNode>();

                if (ctx.interfaceDeclaration() is { } interfaceDeclCtx)
                    return new TypeNameNode(interfaceDeclCtx.Start.Line, interfaceDeclCtx.IDENTIFIER().GetText());

                if (ctx.annotationTypeDeclaration() is { })
                    throw new NotImplementedException("annotation type declaration");

                if (ctx.classDeclaration() is { } clsDeclCtx)
                    return new TypeNameNode(clsDeclCtx.Start.Line, clsDeclCtx.IDENTIFIER().GetText());

                if (ctx.enumDeclaration() is { } enumDeclCtx)
                    return new TypeNameNode(enumDeclCtx.Start.Line, enumDeclCtx.IDENTIFIER().GetText());

                // unreachable path
                throw new SyntaxErrorException("Source file contained unexpected content");
            }

        }

        public override ASTNode VisitInterfaceMemberDeclaration([NotNull] InterfaceMemberDeclarationContext ctx)
            => base.Visit(ctx.children.Single());

        public override ASTNode VisitConstDeclaration([NotNull] ConstDeclarationContext ctx)
            => new DeclListNode(ctx.Start.Line, ctx.constantDeclarator().Select(
                constDeclCtx => this.Visit(constDeclCtx).As<DeclNode>()));

        public override ASTNode VisitConstantDeclarator([NotNull] ConstantDeclaratorContext ctx)
        {
            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            if (ctx.LBRACK().Length > 0)
                throw new NotImplementedException("arrays");

            ExprNode init = this.Visit(ctx.variableInitializer()).As<ExprNode>();

            return new VarDeclNode(ctx.Start.Line, identifier, init);
        }

        public override ASTNode VisitInterfaceMethodDeclaration([NotNull] InterfaceMethodDeclarationContext ctx)
        {
            var identifier = new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());

            if (ctx.annotation() is { } && ctx.annotation().Any())
                throw new NotImplementedException("annotations");

            TypeNameListNode? templateArgs = null;
            if (ctx.typeParameters() is { })
                templateArgs = this.Visit(ctx.typeParameters()).As<TypeNameListNode>();

            FuncParamsNode @params = this.Visit(ctx.formalParameters()).As<FuncParamsNode>();

            // brackets applies to the return type, historical reasons
            if (ctx.LBRACK().Length > 0)
                throw new NotImplementedException("brackets after method definition");

            if (ctx.THROWS() is { })
                throw new NotImplementedException("exceptions");

            BlockStatNode body = this.Visit(ctx.methodBody()).As<BlockStatNode>();

            return new FuncDeclNode(ctx.Start.Line, identifier,
                templateArgs ?? new TypeNameListNode(ctx.Start.Line),
                @params, body);
        }

        public override ASTNode VisitGenericInterfaceMethodDeclaration([NotNull] GenericInterfaceMethodDeclarationContext ctx)
        {
            TypeNameListNode templateArgs = this.Visit(ctx.typeParameters()).As<TypeNameListNode>();
            FuncDeclNode func = this.Visit(ctx.interfaceMethodDeclaration()).As<FuncDeclNode>();

            // throwing exception to suppress warning
            return new FuncDeclNode(ctx.Start.Line, func.IdentifierNode,
                new TypeNameListNode(templateArgs.Line, templateArgs.Types.Concat(func.TemplateArgs.Types)),
                func.ParametersNode ?? throw new SyntaxErrorException("Unknown construct"),
                func.Definition ?? throw new SyntaxErrorException("Unknown construct"));
        }

        #endregion

        #region variable declarators

        public override ASTNode VisitVariableDeclarators([NotNull] VariableDeclaratorsContext ctx)
            => new DeclListNode(ctx.Start.Line, ctx.variableDeclarator().Select(
                varDeclCtx => this.Visit(varDeclCtx).As<DeclNode>()));

        public override ASTNode VisitVariableDeclarator([NotNull] VariableDeclaratorContext ctx)
        {
            VariableDeclaratorIdContext varDeclIdCtx = ctx.variableDeclaratorId();
            IdNode identifier = this.Visit(varDeclIdCtx).As<IdNode>();

            if (ctx.variableInitializer() is { } varInitCtx) {
                ExprNode init = this.Visit(varInitCtx).As<ExprNode>();
                return new VarDeclNode(ctx.Start.Line, identifier, init);
            }

            return new VarDeclNode(ctx.Start.Line, identifier);
        }

        public override ASTNode VisitVariableDeclaratorId([NotNull] VariableDeclaratorIdContext ctx)
        {
            if (ctx.LBRACK().Length > 0)
                throw new NotImplementedException("arrays");

            return new IdNode(ctx.Start.Line, ctx.IDENTIFIER().GetText());
        }

        #endregion

        #region local declarations

        public override ASTNode VisitLocalVariableDeclaration([NotNull] LocalVariableDeclarationContext ctx)
        {
            string modifiers = "";
            int? declSpecsStartLine = null;
            if (ctx.variableModifier() is { } varModifierCtxList && varModifierCtxList.Any()) {
                modifiers = string.Join(" ",
                    varModifierCtxList.Select(modCtx => this.ProcessVariableModifier(modCtx)));
                declSpecsStartLine = varModifierCtxList.First().Start.Line;
            }

            TypeNameNode typeName = this.Visit(ctx.typeType()).As<TypeNameNode>();
            declSpecsStartLine ??= typeName.Line;
            var declSpecs = new DeclSpecsNode(declSpecsStartLine ?? ctx.Start.Line, modifiers, typeName);
            DeclListNode declList = this.Visit(ctx.variableDeclarators()).As<DeclListNode>();

            return new DeclStatNode(ctx.Start.Line, declSpecs, declList);
        }

        public override ASTNode VisitLocalTypeDeclaration([NotNull] LocalTypeDeclarationContext ctx)
        {
            if (ctx.SEMI() is { })
                return new EmptyStatNode(ctx.Start.Line);

            string modifiers = "";
            int? declSpecsStartLine = null;
            if (ctx.classOrInterfaceModifier() is { } clssOrInterfaceModList &&
                clssOrInterfaceModList.Any()) {
                modifiers = string.Join(" ", clssOrInterfaceModList.Select(
                    modCtx => this.ProcessClassOrInterfaceModifier(modCtx)));
                declSpecsStartLine = clssOrInterfaceModList.First().Start.Line;
            }

            if (ctx.classDeclaration() is { }) {
                TypeDeclNode decl = this.Visit(ctx.classDeclaration()).As<TypeDeclNode>();
                declSpecsStartLine ??= decl.Line;
                var declSpecs = new DeclSpecsNode(declSpecsStartLine ?? ctx.Start.Line, modifiers, decl.Identifier);
                return new ClassNode(ctx.Start.Line, declSpecs, decl);
            }

            if (ctx.interfaceDeclaration() is { }) {
                TypeDeclNode decl = this.Visit(ctx.interfaceDeclaration()).As<TypeDeclNode>();
                declSpecsStartLine ??= decl.Line;
                var declSpecs = new DeclSpecsNode(declSpecsStartLine ?? ctx.Start.Line, modifiers, decl.Identifier);
                return new InterfaceNode(ctx.Start.Line, declSpecs, decl);
            }

            // unreachable path
            throw new SyntaxErrorException("Source file contained unexpected content");
        }

        #endregion

        #region annotation declarations

        public override ASTNode VisitAnnotationTypeDeclaration([NotNull] AnnotationTypeDeclarationContext ctx)
            => throw new NotImplementedException("Declaring an annotation type");

        public override ASTNode VisitAnnotationTypeElementDeclaration([NotNull] AnnotationTypeElementDeclarationContext ctx)
            => throw new NotImplementedException("Declaring an annotation type");

        #endregion

        #region private methods instead of visiting Modifier Contexts

        private string ProcessModifier(ModifierContext modifierCtx)
        {
            if (modifierCtx.classOrInterfaceModifier() is { } classOrInterfaceModCtx)
                return this.ProcessClassOrInterfaceModifier(classOrInterfaceModCtx);

            return modifierCtx.GetText();
        }

        private string ProcessClassOrInterfaceModifier([NotNull] ClassOrInterfaceModifierContext classOrInterfaceModCtx)
        {
            if (classOrInterfaceModCtx.annotation() is { })
                throw new NotImplementedException("annotations");

            return classOrInterfaceModCtx.GetText();
        }

        private string ProcessInterfaceMethodModifier([NotNull] InterfaceMethodModifierContext interfaceMethodModCtx)
        {
            if (interfaceMethodModCtx.annotation() is { })
                throw new NotImplementedException("annotations");

            return interfaceMethodModCtx.GetText();
        }

        private string ProcessVariableModifier(VariableModifierContext variableModifierCtx)
        {
            if (variableModifierCtx.annotation() is { })
                throw new NotImplementedException("annotations");

            return variableModifierCtx.GetText();
        }

        #endregion

        #region other (overriden just for the purposes of testing the above methods)

        public override ASTNode VisitQualifiedName([NotNull] QualifiedNameContext ctx)
            => new IdNode(ctx.Start.Line,
                string.Join('.', ctx.IDENTIFIER().Select(id => id.GetText())));

        public override ASTNode VisitClassBody([NotNull] ClassBodyContext ctx)
            => new BlockStatNode(ctx.Start.Line);

        public override ASTNode VisitInterfaceBody([NotNull] InterfaceBodyContext ctx)
            => new BlockStatNode(ctx.Start.Line);

        public override ASTNode VisitMethodBody([NotNull] MethodBodyContext ctx)
            => new BlockStatNode(ctx.Start.Line);

        public override ASTNode VisitFormalParameters([NotNull] FormalParametersContext ctx)
            => new FuncParamsNode(ctx.Start.Line);

        #endregion
    }
}
