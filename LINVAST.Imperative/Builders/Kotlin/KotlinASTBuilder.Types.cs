using System;
using System.Linq;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule:
        // classDeclaration : modifierList? (CLASS | INTERFACE) NL* simpleIdentifier
        //                       (NL* typeParameters)? (NL* primaryConstructor)?
        //                       (NL* COLON NL* delegationSpecifiers)? (NL* typeConstraints)?
        //                       (NL* classBody | NL* enumClassBody)?
        //
        // TODO: support class declarations
        public override ASTNode VisitClassDeclaration(KotlinParser.ClassDeclarationContext ctx)
        {
            throw new NotImplementedException("class declaration is not supported");
        }

        // Grammar rule: userType : simpleUserType (NL* DOT NL* simpleUserType)*
        // e.g. val x: Int or val list: List<String>
        public override ASTNode VisitUserType(KotlinParser.UserTypeContext ctx)
        {
            var fullTypeName = string.Join(".", ctx.simpleUserType().Select(t => this.Visit(t).As<TypeNameNode>().TypeName));
            return new TypeNameNode(ctx.Start.Line, fullTypeName);
        }

        // Grammar rule: simpleUserType : simpleIdentifier (NL* typeArguments)?
        // e.g. val list: List<String>
        // ignore type arguments (generics) for now
        public override ASTNode VisitSimpleUserType(KotlinParser.SimpleUserTypeContext ctx)
        {
            return new TypeNameNode(ctx.Start.Line, ctx.simpleIdentifier().GetText());
        }

        // Grammar rule: nullableType : (typeReference | parenthesizedType) NL* QUEST+
        // e.g. val name: String? or val age:Int?
        public override ASTNode VisitNullableType(KotlinParser.NullableTypeContext ctx)
        {
            var innerTypeText = ctx.typeReference()?.GetText() ?? ctx.parenthesizedType()?.GetText();
            return new TypeNameNode(ctx.Start.Line, innerTypeText + "?");
        }

        // Grammar rule: typeReference : LPAREN typeReference RPAREN | userType | DYNAMIC
        // val x: Int            ===> userType → plain type name
        // val x: kotlin.io.File ===> userType → qualified name
        // val x: (Int)          ===> LPAREN typeReference RPAREN → strips parens recursively
        // val x: dynamic        ===> parsed as userType (ordinary identifier) by the lexer
        public override ASTNode VisitTypeReference(KotlinParser.TypeReferenceContext ctx)
        {
            if (ctx.userType() != null) return this.Visit(ctx.userType());
            if (ctx.typeReference() != null) return this.Visit(ctx.typeReference());
            throw new NotImplementedException("dynamic is not supported");
        }

        // Grammar rule: functionType : (functionTypeReceiver NL* DOT NL*)? functionTypeParameters NL* ARROW type
        // e.g. () -> Unit, (Int) -> String, String.(Int) -> Unit
        // TODO: support function types
        public override ASTNode VisitFunctionType(KotlinParser.FunctionTypeContext ctx)
        {
            throw new NotImplementedException("function types are not supported");
        }
    }
}
