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
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // wires up: KotlinLexer → CommonTokenStream → KotlinParser
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

        public ASTNode BuildFromSource(string code)
        {
            return this.Visit(this.CreateParser(code).kotlinFile());
        }

        // helper when wanting to start parsing from a certain point in the code
        public ASTNode BuildFromSource(string code, Func<KotlinParser, ParserRuleContext> entryProvider)
        {
            return this.Visit(entryProvider(this.CreateParser(code)));
        }

        public override ASTNode Visit(IParseTree tree)
        {
            LogObj.Visit(tree as ParserRuleContext); //logging current rule being visited
            try {
                return base.Visit(tree);
            } catch (NullReferenceException e){
                throw new SyntaxErrorException("Source file contained unexpected content", e);
            }
        }

        public override ASTNode VisitKotlinFile(KotlinParser.KotlinFileContext ctx)
        {
            var imports = ctx.preamble().importList().importHeader().Select(this.Visit);
            var declarations = ctx.topLevelObject().Select(this.Visit);
            return new SourceNode(imports.Concat(declarations));

        }

        public override ASTNode VisitTopLevelObject(KotlinParser.TopLevelObjectContext ctx)
        {
            if(ctx.classDeclaration() != null) return this.Visit(ctx.classDeclaration());
            if(ctx.objectDeclaration() != null) return this.Visit(ctx.objectDeclaration());
            if(ctx.functionDeclaration() != null) return this.Visit(ctx.functionDeclaration());
            if(ctx.propertyDeclaration() != null) return this.Visit(ctx.propertyDeclaration());
            if(ctx.typeAlias() != null) return this.Visit(ctx.typeAlias());
            throw new NotImplementedException($"Unsupported top-level object: {ctx.GetText()}");
        }

        public override ASTNode VisitStatement(KotlinParser.StatementContext ctx)
        {
            if (ctx.declaration() != null) return this.Visit(ctx.declaration());
            if (ctx.blockLevelExpression() != null) return this.Visit(ctx.blockLevelExpression());
            throw new NotImplementedException($"Unsupported statement: {ctx.GetText()}");
        }

        public override ASTNode VisitBlockLevelExpression(KotlinParser.BlockLevelExpressionContext ctx)
        {
            return this.Visit(ctx.expression());
        }

        public override ASTNode VisitBlock(KotlinParser.BlockContext ctx)
        {
            var statements = ctx.statements().statement().Select(this.Visit);
            return new BlockStatNode(ctx.Start.Line, statements);
        }
    }
}
