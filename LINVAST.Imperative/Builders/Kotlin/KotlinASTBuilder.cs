using System;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Logging;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    [ASTBuilder(".kt")]
    /// <summary>
    /// Builds a Kotlin language AST from source code.
    /// </summary>
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        /// <summary>
        /// Creates an ANTLR parser for the given Kotlin source code.
        /// </summary>
        /// <param name="code">The source code to parse.</param>
        /// <returns>An ANTLR parser configured for the source code.</returns>
        public KotlinParser CreateParser(string code)
        {
            ICharStream stream = CharStreams.fromstring(code);
            var lexer = new KotlinLexer(stream);
            lexer.AddErrorListener(new ThrowExceptionErrorListener());

            ITokenStream tokens = new CommonTokenStream(lexer);
            var parser = new KotlinParser(tokens);
            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowExceptionErrorListener());

            return parser;
        }

        /// <summary>
        /// Builds an AST from the given Kotlin source code.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code)
        {
            return this.Visit(this.CreateParser(code).kotlinFile());
        }

        /// <summary>
        /// Builds an AST from the given Kotlin source code using a custom entry point.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <param name="entryProvider">A function that selects the entry point context from the parser.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code, Func<KotlinParser, ParserRuleContext> entryProvider)
        {
            return this.Visit(entryProvider(this.CreateParser(code)));
        }

        /// <summary>
        /// Visits a parse tree and returns the corresponding AST node.
        /// </summary>
        /// <param name="tree">The parse tree to visit.</param>
        /// <returns>The AST node resulting from visiting the parse tree.</returns>
        /// <exception cref="SyntaxErrorException">Thrown when the source file contains unexpected content.</exception>
        public override ASTNode Visit(IParseTree tree)
        {
            LogObj.Visit(tree as ParserRuleContext);
            try {
                return base.Visit(tree);
            } catch (NullReferenceException e){
                throw new SyntaxErrorException("Source file contained unexpected content", e);
            }
        }

        /// <summary>
        /// Visits the kotlinFile parse tree context.
        /// </summary>
        /// <param name="ctx">The kotlinFile parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitKotlinFile(KotlinParser.KotlinFileContext ctx)
        {
            var imports = ctx.preamble().importList().importHeader().Select(this.Visit);
            var declarations = ctx.topLevelObject().Select(this.Visit);
            return new SourceNode(imports.Concat(declarations));

        }

        /// <summary>
        /// Visits the topLevelObject parse tree context.
        /// </summary>
        /// <param name="ctx">The topLevelObject parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTopLevelObject(KotlinParser.TopLevelObjectContext ctx)
        {
            if(ctx.classDeclaration() != null) return this.Visit(ctx.classDeclaration());
            if(ctx.objectDeclaration() != null) return this.Visit(ctx.objectDeclaration());
            if(ctx.functionDeclaration() != null) return this.Visit(ctx.functionDeclaration());
            if(ctx.propertyDeclaration() != null) return this.Visit(ctx.propertyDeclaration());
            if(ctx.typeAlias() != null) return this.Visit(ctx.typeAlias());
            throw new NotImplementedException($"Unsupported top-level object: {ctx.GetText()}");
        }

        /// <summary>
        /// Visits the statement parse tree context.
        /// </summary>
        /// <param name="ctx">The statement parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitStatement(KotlinParser.StatementContext ctx)
        {
            if (ctx.declaration() != null) return this.Visit(ctx.declaration());
            if (ctx.blockLevelExpression() != null) return this.Visit(ctx.blockLevelExpression());
            throw new NotImplementedException($"Unsupported statement: {ctx.GetText()}");
        }

        /// <summary>
        /// Visits the blockLevelExpression parse tree context.
        /// </summary>
        /// <param name="ctx">The blockLevelExpression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitBlockLevelExpression(KotlinParser.BlockLevelExpressionContext ctx)
        {
            return this.Visit(ctx.expression());
        }

        /// <summary>
        /// Visits the block parse tree context.
        /// </summary>
        /// <param name="ctx">The block parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitBlock(KotlinParser.BlockContext ctx)
        {
            var statements = ctx.statements().statement().Select(this.Visit);
            return new BlockStatNode(ctx.Start.Line, statements);
        }

        private FuncCallExprNode MarkerExpression(int line, string marker, params ExprNode[] args) =>
            args.Any()
                ? new FuncCallExprNode(line, new IdNode(line, marker), new ExprListNode(line, args))
                : new FuncCallExprNode(line, new IdNode(line, marker));

        private ExprStatNode MarkerStatement(int line, string marker, params ExprNode[] args) =>
            new ExprStatNode(line, this.MarkerExpression(line, marker, args));

        private ExprNode AsExprNode(ASTNode node)
        {
            if (node is ExprNode expr) return expr;
            int line = node.Line;
            return new LambdaFuncExprNode(line, new BlockStatNode(line, new[] { node }));
        }
    }
}
