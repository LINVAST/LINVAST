using System;
using System.Linq;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: userType : simpleUserType (NL* DOT NL* simpleUserType)*
        // e.g. val x: Int or val list: List<String>
        /// <summary>
        /// Visits the user type parse tree context.
        /// </summary>
        /// <param name="ctx">The user type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitUserType(KotlinParser.UserTypeContext ctx)
        {
            var fullTypeName = string.Join(".", ctx.simpleUserType().Select(t => this.Visit(t).As<TypeNameNode>().TypeName));
            return new TypeNameNode(ctx.Start.Line, fullTypeName);
        }

        // Grammar rule: simpleUserType : simpleIdentifier (NL* typeArguments)?
        // e.g. val list: List<String>
        // ignore type arguments (generics) for now
        /// <summary>
        /// Visits the simple user type parse tree context.
        /// </summary>
        /// <param name="ctx">The simple user type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSimpleUserType(KotlinParser.SimpleUserTypeContext ctx)
        {
            return new TypeNameNode(ctx.Start.Line, ctx.simpleIdentifier().GetText());
        }

        // Grammar rule: nullableType : (typeReference | parenthesizedType) NL* QUEST+
        // e.g. val name: String? or val age:Int?
        /// <summary>
        /// Visits the nullable type parse tree context.
        /// </summary>
        /// <param name="ctx">The nullable type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
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
        /// <summary>
        /// Visits the type reference parse tree context.
        /// </summary>
        /// <param name="ctx">The type reference parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeReference(KotlinParser.TypeReferenceContext ctx)
        {
            if (ctx.userType() != null) return this.Visit(ctx.userType());
            if (ctx.typeReference() != null) return this.Visit(ctx.typeReference());
            throw new NotImplementedException("dynamic is not supported");
        }

        // Grammar rule: functionType : (functionTypeReceiver NL* DOT NL*)? functionTypeParameters NL* ARROW type
        // e.g. () -> Unit, (Int) -> String, String.(Int) -> Unit
        /// <summary>
        /// Visits the function type parse tree context.
        /// </summary>
        /// <param name="ctx">The function type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionType(KotlinParser.FunctionTypeContext ctx)
        {
            int line = ctx.Start.Line;
            string text = ctx.GetText();
            return new TypeNameNode(line, text);
        }
    }
}
