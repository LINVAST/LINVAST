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
            var parts = ctx.simpleUserType();
            if (parts.Length == 1)
                return this.Visit(parts[0]);

            // Only Kotlin's built-ins may lose their namespace. Keep custom qualified names intact.
            if (parts.Length == 2 && parts[0].GetText() == "kotlin"
                && IsBuiltinType(parts[1].simpleIdentifier().GetText()))
                return this.Visit(parts[1]);

            var types = parts.Select(p => this.BuildSimpleUserType(p, false)).ToArray();
            string prefix = string.Join(".", types.Take(types.Length - 1).Select(t => t.GetText()));
            var last = types.Last();
            return new TypeNameNode(ctx.Start.Line, prefix + "." + last.TypeName, last.TemplateArguments);
        }

        // Grammar rule: simpleUserType : simpleIdentifier (NL* typeArguments)?
        // e.g. val list: List<String>
        /// <summary>
        /// Visits the simple user type parse tree context.
        /// </summary>
        /// <param name="ctx">The simple user type parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSimpleUserType(KotlinParser.SimpleUserTypeContext ctx)
            => this.BuildSimpleUserType(ctx, true);

        private TypeNameNode BuildSimpleUserType(KotlinParser.SimpleUserTypeContext ctx, bool normalizeBuiltin)
        {
            string name = ctx.simpleIdentifier().GetText();
            var arguments = ctx.typeArguments()?.typeProjection()
                .Select(p => this.Visit(p).As<TypeNameNode>()).ToArray() ?? Array.Empty<TypeNameNode>();

            TypeNameNode type;
            if (normalizeBuiltin && name == "Array" && arguments.Length == 1) {
                var element = arguments[0];
                string elementName = ctx.typeArguments().typeProjection(0).type()?.functionType() != null
                    ? "(" + element.TypeName + ")"
                    : element.TypeName;
                type = new TypeNameNode(ctx.Start.Line, elementName + "[]", element.TemplateArguments);
            } else {
                type = new TypeNameNode(ctx.Start.Line, normalizeBuiltin ? NormalizeBuiltinType(name) : name, arguments);
            }

            // This grammar consumes the nullable suffix of a generic type in typeArguments.
            return ctx.typeArguments()?.QUEST() != null
                ? new TypeNameNode(ctx.Start.Line, type.TypeName + "?", type.TemplateArguments)
                : type;
        }

        private static bool IsBuiltinType(string name)
            => name == "Array" || NormalizeBuiltinType(name) != name;

        private static string NormalizeBuiltinType(string name) => name switch
        {
            "Int" => "int",
            "Long" => "long",
            "Short" => "short",
            "Byte" => "byte",
            "Float" => "float",
            "Double" => "double",
            "Boolean" => "bool",
            "Char" => "char",
            "String" => "string",
            "Any" => "object",
            "Unit" => "void",
            "IntArray" => "int[]",
            "LongArray" => "long[]",
            "ShortArray" => "short[]",
            "ByteArray" => "byte[]",
            "FloatArray" => "float[]",
            "DoubleArray" => "double[]",
            "BooleanArray" => "bool[]",
            "CharArray" => "char[]",
            _ => name,
        };

        /// <summary>Visits a generic type argument, preserving variance and star projections.</summary>
        public override ASTNode VisitTypeProjection(KotlinParser.TypeProjectionContext ctx)
        {
            if (ctx.MULT() != null)
                return new TypeNameNode(ctx.Start.Line, "*");

            var type = this.Visit(ctx.type()).As<TypeNameNode>();
            return ctx.typeProjectionModifierList() != null
                ? new TypeNameNode(ctx.Start.Line, ctx.typeProjectionModifierList().GetText() + " " + type.TypeName, type.TemplateArguments)
                : type;
        }

        /// <summary>Visits the type enclosed in parentheses.</summary>
        public override ASTNode VisitParenthesizedType(KotlinParser.ParenthesizedTypeContext ctx)
        {
            var type = this.Visit(ctx.type()).As<TypeNameNode>();
            // Function types need grouping when used as a nullable type or a receiver.
            return ctx.type().functionType() != null
                ? new TypeNameNode(ctx.Start.Line, "(" + type.GetText() + ")")
                : type;
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
            var innerType = (ctx.typeReference() != null
                ? this.Visit(ctx.typeReference())
                : this.Visit(ctx.parenthesizedType())).As<TypeNameNode>();
            return new TypeNameNode(ctx.Start.Line, innerType.TypeName + "?", innerType.TemplateArguments);
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
            string receiver = ctx.functionTypeReceiver() != null
                ? this.Visit(ctx.functionTypeReceiver()).As<TypeNameNode>().GetText() + "."
                : "";
            var parameters = ctx.functionTypeParameters().children
                .Where(c => c is KotlinParser.TypeContext || c is KotlinParser.ParameterContext)
                .Select(c => c is KotlinParser.ParameterContext p
                    ? p.simpleIdentifier().GetText() + ": " + this.Visit(p.type()).As<TypeNameNode>().GetText()
                    : this.Visit(c).As<TypeNameNode>().GetText());
            string text = receiver + "(" + string.Join(", ", parameters) + ") -> "
                + this.Visit(ctx.type()).As<TypeNameNode>().GetText();
            return new TypeNameNode(line, text);
        }
    }
}
