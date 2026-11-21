using System;
using System.Linq;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: functionDeclaration : functionModifierList? FUN (NL* type NL* DOT)? (NL* typeParameters)? (NL* receiverType NL* DOT)? (
        //                                          NL* identifier
        //                                    )? NL* functionValueParameters (NL* COLON NL* type)? (NL* typeConstraints)? (NL* functionBody)?
        //
        // Example: fun add(x: Int, y: Int): Int { return x }
        /// <summary>
        /// Visits the function declaration parse tree context.
        /// </summary>
        /// <param name="ctx">The function declaration parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionDeclaration(KotlinParser.FunctionDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            IdNode name = new IdNode(line, ctx.identifier().GetText());
            TypeNameNode returnType = ctx.COLON() != null
                ? this.Visit(ctx.type().Last()).As<TypeNameNode>()
                : new TypeNameNode(line, ctx.functionBody()?.expression() != null ? "var" : "void");
            
            DeclSpecsNode declSpecs = new DeclSpecsNode(line, returnType);
            
            FuncParamsNode funcParams = this.Visit(ctx.functionValueParameters()).As<FuncParamsNode>();
            BlockStatNode? body = ctx.functionBody() != null
                                ? this.Visit(ctx.functionBody()).As<BlockStatNode>()
                                : null;
            FuncDeclNode decl = body is null
                ? new FuncDeclNode(line, name, funcParams)
                : new FuncDeclNode(line, name, funcParams, body);

            return new FuncNode(line, declSpecs, decl);
        }

        // Grammar rule: functionBody : block | ASSIGNMENT NL* expression
        /// <summary>
        /// Visits the function body parse tree context.
        /// </summary>
        /// <param name="ctx">The function body parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionBody(KotlinParser.FunctionBodyContext ctx)
        {
            if (ctx.block() != null) return this.Visit(ctx.block());
            int line = ctx.Start.Line;
            ExprNode expr = this.AsExprNode(this.Visit(ctx.expression()));
            return new BlockStatNode(line, new JumpStatNode(line, expr));
        }

        // Grammar rule: functionValueParameters : LPAREN (functionValueParameter (COMMA functionValueParameter)* COMMA?)? RPAREN
        /// <summary>
        /// Visits the function value parameters parse tree context.
        /// </summary>
        /// <param name="ctx">The function value parameters parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionValueParameters(KotlinParser.FunctionValueParametersContext ctx)
        {
            int line = ctx.Start.Line;
            FuncParamNode[] funcParams = ctx.functionValueParameter()
                                    .Select(p => this.Visit(p).As<FuncParamNode>())
                                    .ToArray();
            return new FuncParamsNode(line, funcParams);
        }

        // Grammar rule: functionValueParameter : modifierList? parameter (ASSIGNMENT expression)? 
        /// <summary>
        /// Visits the function value parameter parse tree context.
        /// </summary>
        /// <param name="ctx">The function value parameter parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionValueParameter(KotlinParser.FunctionValueParameterContext ctx)
        {
            FuncParamNode parameter = this.Visit(ctx.parameter()).As<FuncParamNode>();
            if (ctx.expression() is null) return parameter;
            return new FuncParamNode(ctx.Start.Line, parameter.Specifiers,
                new VarDeclNode(ctx.Start.Line, new IdNode(ctx.Start.Line, parameter.Declarator.Identifier),
                    this.AsExprNode(this.Visit(ctx.expression()))));
        }

        // Grammar rule: parameter : simpleIdentifier COLON type 
        /// <summary>
        /// Visits the parameter parse tree context.
        /// </summary>
        /// <param name="ctx">The parameter parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParameter(KotlinParser.ParameterContext ctx)
        {
            int line = ctx.Start.Line;
            TypeNameNode type = this.Visit(ctx.type()).As<TypeNameNode>();
            DeclSpecsNode specs = new DeclSpecsNode(line, type);

            IdNode idNode = new IdNode(line, ctx.simpleIdentifier().GetText());
            VarDeclNode decl = new VarDeclNode(line, idNode);
            return new FuncParamNode(line, specs, decl);
        }
    }
}
