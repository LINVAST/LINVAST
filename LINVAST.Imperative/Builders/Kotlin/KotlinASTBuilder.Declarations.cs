using System;
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
    }
}
