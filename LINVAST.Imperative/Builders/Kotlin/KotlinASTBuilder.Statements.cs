using System;
using System.Collections.Generic;
using System.Linq;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: ifExpression : IF NL* LPAREN expression RPAREN NL* controlStructureBody?
        //                              SEMICOLON? (NL* ELSE NL* controlStructureBody?)?
        /// <summary>
        /// Visits the if expression parse tree context.
        /// </summary>
        /// <param name="ctx">The if expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitIfExpression(KotlinParser.IfExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode condition = this.AsExprNode(this.Visit(ctx.expression()));
            StatNode thenBody = this.Visit(ctx.controlStructureBody(0)).As<StatNode>();
            if (ctx.controlStructureBody().Length > 1) {
                StatNode elseBody = this.Visit(ctx.controlStructureBody(1)).As<StatNode>();
                return new IfStatNode(line, condition, thenBody, elseBody);
            }
            return new IfStatNode(line, condition, thenBody);
        }

        // Grammar rule: controlStructureBody : block | expression
        /// <summary>
        /// Visits the control structure body parse tree context.
        /// </summary>
        /// <param name="ctx">The control structure body parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitControlStructureBody(KotlinParser.ControlStructureBodyContext ctx)
        {
            int line = ctx.Start.Line;
            if(ctx.block() != null) return this.Visit(ctx.block());
            else return new ExprStatNode(line, this.AsExprNode(this.Visit(ctx.expression())));
        }

        // Grammar rule: whileExpression : WHILE NL* LPAREN expression RPAREN NL* controlStructureBody?
        /// <summary>
        /// Visits the while expression parse tree context.
        /// </summary>
        /// <param name="ctx">The while expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitWhileExpression(KotlinParser.WhileExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode condition = this.AsExprNode(this.Visit(ctx.expression()));
            StatNode body = ctx.controlStructureBody() != null
                            ? this.Visit(ctx.controlStructureBody()).As<StatNode>()
                            : new BlockStatNode(line); // empty body
            return new WhileStatNode(line, condition, body);
        }


        // Grammar rule: doWhileExpression : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
        /// <summary>
        /// Visits the do while expression parse tree context.
        /// </summary>
        /// <param name="ctx">The do while expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitDoWhileExpression(KotlinParser.DoWhileExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode condition = this.AsExprNode(this.Visit(ctx.expression()));
            StatNode body = ctx.controlStructureBody() != null
                ? this.Visit(ctx.controlStructureBody()).As<StatNode>()
                : new BlockStatNode(line);
            return new LabeledStatNode(line, "__linvast_do_while", new WhileStatNode(line, condition, body));
        }


        // Grammar rule: whenExpression : WHEN (NL* (LPAREN expression RPAREN)? NL* LCURL whenEntry* RCURL)?
        /// <summary>
        /// Visits the when expression parse tree context.
        /// </summary>
        /// <param name="ctx">The when expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitWhenExpression(KotlinParser.WhenExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode condition = ctx.expression() != null
                ? this.AsExprNode(this.Visit(ctx.expression()))
                : new LitExprNode(line, true);
            var cases = new List<LabeledStatNode>();
            foreach (var entry in ctx.whenEntry())
            {
                LabeledStatNode caseNode = this.Visit(entry).As<LabeledStatNode>();
                cases.Add(caseNode);
            }
            return new SwitchStatNode(line, condition, new BlockStatNode(line, cases));
        }

        // Grammar rule: whenEntry : whenCondition (NL* COMMA NL* whenCondition)* (ARROW controlStructureBody)?
        //                            | ELSE ARROW controlStructureBody
        /// <summary>
        /// Visits the when entry parse tree context.
        /// </summary>
        /// <param name="ctx">The when entry parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitWhenEntry(KotlinParser.WhenEntryContext ctx)
        {
            int line = ctx.Start.Line;
            StatNode body = ctx.controlStructureBody() != null
                ? this.Visit(ctx.controlStructureBody()).As<StatNode>()
                : new BlockStatNode(line);

            if (ctx.ELSE() != null)
            {
                return new LabeledStatNode(line, "default", body);
            }

            var conditions = ctx.whenCondition().Select(this.Visit).ToArray();
            if (conditions.Length == 0)
            {
                return new LabeledStatNode(line, "default", body);
            }

            string labelText = conditions.Length == 1
                ? $"case {conditions[0].GetText()}"
                : $"case {{{string.Join(", ", conditions.Select(c => c.GetText()))}}}";

            return new LabeledStatNode(line, labelText, body);
        }

        // Grammar rule: whenCondition : expression | rangeTest | typeTest
        /// <summary>
        /// Visits the when condition parse tree context.
        /// </summary>
        /// <param name="ctx">The when condition parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitWhenCondition(KotlinParser.WhenConditionContext ctx)
        {
            if (ctx.expression() != null) return this.Visit(ctx.expression());
            if (ctx.rangeTest() != null) return this.Visit(ctx.rangeTest());
            if (ctx.typeTest() != null) return this.Visit(ctx.typeTest());
            throw new NotImplementedException("unsupported when condition");
        }

        // Grammar rule: rangeTest : inOperator NL* expression
        /// <summary>
        /// Visits the range test parse tree context.
        /// </summary>
        /// <param name="ctx">The range test parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitRangeTest(KotlinParser.RangeTestContext ctx)
        {
            int line = ctx.Start.Line;
            string opText = ctx.inOperator().GetText();
            bool negate = opText.Contains("!");
            ExprNode range = this.AsExprNode(this.Visit(ctx.expression()));
            string opSymbol = negate ? "!in" : "in";
            var op = new RelOpNode(line, opSymbol, (x, y) => negate ^ (x?.Equals(y) ?? false || (y is IEnumerable<object> e && e.Contains(x))));
            return new RelExprNode(line, range, op, range);
        }

        // Grammar rule: typeTest : isOperator NL* type
        /// <summary>
        /// Visits the type test parse tree context.
        /// </summary>
        /// <param name="ctx">The type test parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeTest(KotlinParser.TypeTestContext ctx)
        {
            int line = ctx.Start.Line;
            string opText = ctx.isOperator().GetText();
            bool negate = opText.Contains("!");
            IdNode typeNode = new IdNode(line, ctx.type().GetText());
            string opSymbol = negate ? "!is" : "is";
            var op = new RelOpNode(line, opSymbol, (x, y) => negate ^ (x?.GetType().Name == y?.ToString()));
            return new RelExprNode(line, typeNode, op, typeNode);
        }

        // Grammar rule: tryExpression : TRY block catchBlock* finallyBlock?
        /// <summary>
        /// Visits the try expression parse tree context.
        /// </summary>
        /// <param name="ctx">The try expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTryExpression(KotlinParser.TryExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            StatNode tryBody = this.Visit(ctx.block()).As<StatNode>();
            var catchClauses = ctx.catchBlock().Select(c => this.Visit(c).As<CatchClauseNode>()).ToArray();
            StatNode? finallyBody = ctx.finallyBlock() != null
                ? this.Visit(ctx.finallyBlock()).As<StatNode>()
                : null;
            return new TryStatNode(line, tryBody, catchClauses, null, finallyBody);
        }

        // Grammar rule: catchBlock : CATCH NL* LPAREN simpleIdentifier COLON userType RPAREN NL* block
        /// <summary>
        /// Visits the catch block parse tree context.
        /// </summary>
        /// <param name="ctx">The catch block parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitCatchBlock(KotlinParser.CatchBlockContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode? exceptionType = ctx.userType() != null
                ? new IdNode(line, ctx.userType().GetText())
                : null;
            IdNode? binding = ctx.simpleIdentifier() != null
                ? new IdNode(line, ctx.simpleIdentifier().GetText())
                : null;
            StatNode body = this.Visit(ctx.block()).As<StatNode>();
            return new CatchClauseNode(line, body, exceptionType, binding);
        }

        // Grammar rule: finallyBlock : FINALLY NL* block
        /// <summary>
        /// Visits the finally block parse tree context.
        /// </summary>
        /// <param name="ctx">The finally block parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFinallyBlock(KotlinParser.FinallyBlockContext ctx)
        {
            return this.Visit(ctx.block());
        }


        // Grammar rule: forExpression : FOR NL* LPAREN annotations*
        //                               (variableDeclaration | multiVariableDeclaration)
        //                               IN expression RPAREN NL* controlStructureBody?
        /// <summary>
        /// Visits the for expression parse tree context.
        /// </summary>
        /// <param name="ctx">The for expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitForExpression(KotlinParser.ForExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode iterable = this.AsExprNode(this.Visit(ctx.expression()));
            StatNode body = ctx.controlStructureBody() != null
                ? this.Visit(ctx.controlStructureBody()).As<StatNode>()
                : new BlockStatNode(line);

            if (ctx.multiVariableDeclaration() != null) {
                DeclStatNode iteratorDecl = this.BuildMultiVarIteratorDecl(ctx.multiVariableDeclaration());
                return new ForeachStatNode(line, iteratorDecl, iterable, body);
            }

            var varDecl = ctx.variableDeclaration();
            TypeNameNode type = new TypeNameNode(line, varDecl.type()?.GetText() ?? "var");
            IdNode iterator = new IdNode(line, varDecl.simpleIdentifier().GetText());
            return new ForeachStatNode(line, type, iterator, iterable, body);
        }

        private DeclStatNode BuildMultiVarIteratorDecl(KotlinParser.MultiVariableDeclarationContext ctx)
        {
            int line = ctx.Start.Line;
            IEnumerable<VarDeclNode> declarators = ctx.variableDeclaration()
                .Select(v => new VarDeclNode(v.Start.Line, new IdNode(v.Start.Line, v.simpleIdentifier().GetText())));
            var declSpecs = new DeclSpecsNode(line, new TypeNameNode(line, "var"));
            var declList = new DeclListNode(line, declarators);
            return new DeclStatNode(line, declSpecs, declList);
        }

        // Grammar rule: jumpExpression : THROW NL* expression
        //                              | (RETURN | RETURN_AT) expression?
        //                              | CONTINUE | CONTINUE_AT | BREAK | BREAK_AT
        /// <summary>
        /// Visits the jump expression parse tree context.
        /// </summary>
        /// <param name="ctx">The jump expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitJumpExpression(KotlinParser.JumpExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            if (ctx.THROW() != null)
            {
                ExprNode throwExpr = this.AsExprNode(this.Visit(ctx.expression()));
                return this.MarkerStatement(line, "__linvast_throw", throwExpr);
            }
            if (ctx.RETURN() != null || ctx.RETURN_AT() != null)
            {
                if (ctx.expression() != null) return new JumpStatNode(line, this.AsExprNode(this.Visit(ctx.expression())));
                return new JumpStatNode(line, JumpStatType.Return);
            }
            if (ctx.BREAK() != null)
            {
                return new JumpStatNode(line, JumpStatType.Break);
            }
            if (ctx.BREAK_AT() != null)
            {
                return this.MarkerStatement(line, "__linvast_break_label", new IdNode(line, ctx.BREAK_AT().GetText()));
            }
            if (ctx.CONTINUE() != null)
            {
                return new JumpStatNode(line, JumpStatType.Continue);
            }
            if (ctx.CONTINUE_AT() != null)
            {
                return this.MarkerStatement(line, "__linvast_continue_label", new IdNode(line, ctx.CONTINUE_AT().GetText()));
            }
            throw new NotImplementedException("unsupported jump expression");
        }

        // Grammar rule: expression : disjunction (assignmentOperator disjunction)*
        // TODO: support assignment operators (=, +=, -=, *=, /=, %=)
        /// <summary>
        /// Visits the expression parse tree context.
        /// </summary>
        /// <param name="ctx">The expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitExpression(KotlinParser.ExpressionContext ctx)
        {
            int line = ctx.Start.Line;   
            if(ctx.assignmentOperator().Length > 0) {
                ExprNode left = this.AsExprNode(this.Visit(ctx.disjunction(0)));
                AssignOpNode op = AssignOpNode.FromSymbol(line, ctx.assignmentOperator(0).GetText());
                ExprNode right = this.AsExprNode(this.Visit(ctx.disjunction(1)));
                return new ExprStatNode(line, new AssignExprNode(line, left, op, right));
            }
            
            return this.Visit(ctx.disjunction(0));
        }
    }
}
