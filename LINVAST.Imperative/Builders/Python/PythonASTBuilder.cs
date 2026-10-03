using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Logging;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Python
{
    [ASTBuilder(".py")]
    /// <summary>
    /// Builds a Python language AST from source code.
    /// </summary>
    public sealed partial class PythonASTBuilder : Python3ParserBaseVisitor<ASTNode>, IASTBuilder<Python3Parser>
    {
        private int comprehensionAccumulatorIndex;
        private readonly List<PendingComprehension> pendingComprehensions = new();

        /// <summary>
        /// Builds an AST from the given Python source code.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code)
        {
            this.pendingComprehensions.Clear();
            try {
                return this.Visit(this.CreateParser(code).file_input());
            } finally {
                this.pendingComprehensions.Clear();
            }
        }

        /// <summary>
        /// Creates an ANTLR parser for the given Python source code.
        /// </summary>
        /// <param name="code">The source code to parse.</param>
        /// <returns>An ANTLR parser configured for the source code.</returns>
        public Python3Parser CreateParser(string code) => this.CreateParser(code, initialLine: 1);

        /// <summary>
        /// Creates an ANTLR parser for the given Python source code with a specified starting line.
        /// </summary>
        /// <param name="code">The source code to parse.</param>
        /// <param name="initialLine">The starting line number for the parser.</param>
        /// <returns>An ANTLR parser configured for the source code.</returns>
        private Python3Parser CreateParser(string code, int initialLine)
        {
            ICharStream stream = CharStreams.fromstring(code);
            var lexer = new Python3Lexer(stream);
            lexer.Line = initialLine;
            lexer.AddErrorListener(new ThrowExceptionErrorListener());
            ITokenStream tokens = new CommonTokenStream(lexer);
            var parser = new Python3Parser(tokens);
            parser.BuildParseTree = true;
            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowExceptionErrorListener());
            return parser;
        }

        /// <summary>
        /// Builds an AST from the given Python source code using a custom entry point.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <param name="entryProvider">A function that selects the entry point context from the parser.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code, Func<Python3Parser, ParserRuleContext> entryProvider)
        {
            this.pendingComprehensions.Clear();
            try {
                return this.Visit(entryProvider(this.CreateParser(code)));
            } finally {
                this.pendingComprehensions.Clear();
            }
        }

        /// <summary>
        /// Builds an AST from the given Python source code using a custom entry point and starting line.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <param name="entryProvider">A function that selects the entry point context from the parser.</param>
        /// <param name="initialLine">The starting line number for the parser.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        private ASTNode BuildFromSource(string code, Func<Python3Parser, ParserRuleContext> entryProvider, int initialLine) =>
            this.Visit(entryProvider(this.CreateParser(code, initialLine)));

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
            } catch (NullReferenceException e) {
                throw new SyntaxErrorException("Source file contained unexpected content", e);
            }
        }

        /// <summary>
        /// Visits the file_input parse tree context.
        /// </summary>
        /// <param name="ctx">The file_input parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFile_input(Python3Parser.File_inputContext ctx)
        {
            IEnumerable<ASTNode> statements = ctx.stmt()
                .Select(this.Visit)
                .SelectMany(node => node is BlockStatNode block
                    ? block.Children.AsEnumerable()
                    : Enumerable.Repeat(node, 1));

            return new SourceNode(this.AddDeclarations(statements));
        }

        /// <summary>
        /// Visits the stmt parse tree context.
        /// </summary>
        /// <param name="ctx">The stmt parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitStmt(Python3Parser.StmtContext ctx)
        {
            if (ctx.simple_stmts() is not null)
                return this.Visit(ctx.simple_stmts());
            return this.Visit(ctx.compound_stmt());
        }

        /// <summary>
        /// Visits the simple_stmts parse tree context.
        /// </summary>
        /// <param name="ctx">The simple_stmts parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSimple_stmts(Python3Parser.Simple_stmtsContext ctx)
        {
            var stmts = ctx.simple_stmt().Select(this.Visit).ToArray();
            if (stmts.Length == 1)
                return stmts[0];
            return new BlockStatNode(ctx.Start.Line, stmts);
        }

        /// <summary>
        /// Visits the simple_stmt parse tree context.
        /// </summary>
        /// <param name="ctx">The simple_stmt parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSimple_stmt(Python3Parser.Simple_stmtContext ctx) =>
            this.Visit(ctx.children.Single(c => c is ParserRuleContext));

        /// <summary>
        /// Creates a variable declaration statement node from the given identifier and optional initializer.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier node.</param>
        /// <param name="initializer">The optional initializer expression.</param>
        /// <param name="typeName">The type name for the declaration specifiers.</param>
        /// <returns>The variable declaration statement node.</returns>
        private static DeclStatNode MakeVarDecl(int line, IdNode identifier, ExprNode? initializer, string typeName)
        {
            var declSpecs = new DeclSpecsNode(line, typeName);
            DeclNode decl = initializer is not null
                ? new VarDeclNode(line, identifier, initializer)
                : new VarDeclNode(line, identifier);
            return new DeclStatNode(line, declSpecs, new DeclListNode(line, decl));
        }

        /// <summary>
        /// Marks the current number of pending comprehensions to enable scoping hoisting.
        /// </summary>
        /// <returns>The current count of pending comprehensions before new ones are added.</returns>
        private int MarkPendingComprehensions() => this.pendingComprehensions.Count;

        /// <summary>
        /// Takes the pending comprehensions that were added after the specified mark.
        /// </summary>
        /// <param name="mark">The mark returned by <see cref="MarkPendingComprehensions"/>.</param>
        /// <returns>The pending comprehensions added after the mark, and removes them from the pending list.</returns>
        private IReadOnlyList<PendingComprehension> TakePendingComprehensions(int mark)
        {
            var items = this.pendingComprehensions.Skip(mark).ToArray();
            this.pendingComprehensions.RemoveRange(mark, this.pendingComprehensions.Count - mark);
            return items;
        }

        /// <summary>
        /// Flattens the hoisted statements from the given pending comprehensions.
        /// </summary>
        /// <param name="comprehensions">The pending comprehensions to flatten.</param>
        /// <returns>The flattened hoisted statements.</returns>
        private static IEnumerable<ASTNode> HoistedStatements(IEnumerable<PendingComprehension> comprehensions) =>
            comprehensions.SelectMany(c => c.Expansion.Children);

        /// <summary>
        /// Hoists pending comprehension statements before the given statement.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="comprehensions">The pending comprehensions to hoist.</param>
        /// <param name="statement">The statement to place after the hoisted comprehensions.</param>
        /// <returns>A block statement containing the hoisted comprehensions followed by the statement.</returns>
        private static BlockStatNode HoistComprehensionsBefore(
            int line,
            IEnumerable<PendingComprehension> comprehensions,
            StatNode statement) =>
            new(line, HoistedStatements(comprehensions).Concat(new ASTNode[] { statement }));

        /// <summary>
        /// Represents a pending comprehension that is hoisted out of an enclosing scope.
        /// </summary>
        private sealed class PendingComprehension
        {
            /// <summary>
            /// Gets the name of the accumulator variable for the comprehension.
            /// </summary>
            public string AccumulatorName { get; }

            /// <summary>
            /// Gets the block statement that expands the comprehension.
            /// </summary>
            public BlockStatNode Expansion { get; }

            /// <summary>
            /// Initializes a new instance of the <see cref="PendingComprehension"/> class.
            /// </summary>
            /// <param name="accumulatorName">The accumulator variable name.</param>
            /// <param name="expansion">The block statement that expands the comprehension.</param>
            public PendingComprehension(string accumulatorName, BlockStatNode expansion)
            {
                this.AccumulatorName = accumulatorName;
                this.Expansion = expansion;
            }
        }

        /// <summary>
        /// Promotes top-level identifier and tuple-unpacking assignments into explicit declarations.
        /// </summary>
        /// <param name="statements">The flattened statement nodes to process.</param>
        /// <returns>The statement nodes with implicit declarations promoted to explicit declaration statements.</returns>
        private IReadOnlyList<ASTNode> AddDeclarations(IEnumerable<ASTNode> statements)
        {
            var nodes = new List<ASTNode>();
            var declared = new HashSet<string>();
            IEnumerable<ASTNode> flattenedStatements = statements
                .SelectMany(stat => stat is BlockStatNode block
                    ? block.Children.AsEnumerable()
                    : Enumerable.Repeat(stat, 1));
            foreach (ASTNode stat in flattenedStatements) {
                if (stat is DeclStatNode declStat) {
                    foreach (DeclNode declarator in declStat.DeclaratorList.Declarators)
                        declared.Add(declarator.Identifier);
                    nodes.Add(stat);
                    continue;
                }

                if (stat is ExprStatNode expr && expr.Expression is AssignExprNode assign) {
                    if (this.HasMultipleStarredTargets(assign.LeftOperand))
                        throw new SyntaxErrorException("multiple starred expressions in assignment");

                    // Try single identifier assignment first
                    if (assign.LeftOperand is IdNode id) {
                        if (!declared.Contains(id.Identifier)) {
                            var declSpecs = new DeclSpecsNode(id.Line);
                            var declList = new DeclListNode(id.Line, new VarDeclNode(id.Line, id, assign.RightOperand));
                            nodes.Add(new DeclStatNode(id.Line, declSpecs, declList));
                            declared.Add(id.Identifier);
                        } else {
                            nodes.Add(stat);
                        }
                    }
                    // Try tuple unpacking assignment
                    else if (this.TryPromoteTupleUnpacking(assign.LeftOperand, assign.RightOperand, assign.Line, nodes, declared)) {
                        // Successfully promoted to declarations
                    } else {
                        // Could not promote, keep as expression statement
                        nodes.Add(stat);
                    }
                } else {
                    nodes.Add(stat);
                }
            }
            return nodes.AsReadOnly();
        }
    }
}
