using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: disjunction : conjunction (NL* DISJ NL* conjunction)*
        /// <summary>
        /// Visits the disjunction parse tree context.
        /// </summary>
        /// <param name="ctx">The disjunction parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitDisjunction(KotlinParser.DisjunctionContext ctx)
        {
            if (ctx.conjunction().Length == 1) return this.Visit(ctx.conjunction(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.conjunction(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.conjunction(1)));
            return new LogicExprNode(line, left, BinaryLogicOpNode.FromSymbol(line, "||"), right);
        }

        // Grammar rule: conjunction : equalityComparison (NL* CONJ NL* equalityComparison)*
        /// <summary>
        /// Visits the conjunction parse tree context.
        /// </summary>
        /// <param name="ctx">The conjunction parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitConjunction(KotlinParser.ConjunctionContext ctx)
        {
            if (ctx.equalityComparison().Length == 1) return this.Visit(ctx.equalityComparison(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.equalityComparison(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.equalityComparison(1)));
            return new LogicExprNode(line, left, BinaryLogicOpNode.FromSymbol(line, "&&"), right);
        }

        // Grammar rule: equalityComparison : comparison (equalityOperation NL* comparison)*
        /// <summary>
        /// Visits the equality comparison parse tree context.
        /// </summary>
        /// <param name="ctx">The equality comparison parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitEqualityComparison(KotlinParser.EqualityComparisonContext ctx)
        {
            if (ctx.comparison().Length == 1) return this.Visit(ctx.comparison(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.comparison(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.comparison(1)));
            var op = RelOpNode.FromSymbol(line, ctx.equalityOperation(0).GetText());
            return new RelExprNode(line, left, op, right);
        }

        // Grammar rule: comparison : namedInfix (comparisonOperator NL* namedInfix)?
        /// <summary>
        /// Visits the comparison parse tree context.
        /// </summary>
        /// <param name="ctx">The comparison parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitComparison(KotlinParser.ComparisonContext ctx)
        {
            if (ctx.namedInfix().Length == 1) return this.Visit(ctx.namedInfix(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.namedInfix(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.namedInfix(1)));
            var op = RelOpNode.FromSymbol(line, ctx.comparisonOperator().GetText());
            return new RelExprNode(line, left, op, right);
        }

        // Grammar rule: namedInfix : elvisExpression ((inOperator NL* elvisExpression)+ | (isOperator NL* type))?
        /// <summary>
        /// Visits the named infix parse tree context.
        /// </summary>
        /// <param name="ctx">The named infix parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitNamedInfix(KotlinParser.NamedInfixContext ctx)
        {
            if (ctx.isOperator() != null || ctx.inOperator().Length > 0)
            {
                int line = ctx.Start.Line;
                ExprNode left = this.AsExprNode(this.Visit(ctx.elvisExpression(0)));
                if (ctx.isOperator() != null)
                {
                    string isText = ctx.isOperator().GetText();
                    bool isNegate = isText.Contains("!");
                    string typeText = ctx.type().GetText();
                    ExprNode right = this.MarkerExpression(line, "__linvast_type_assert", 
                        new IdNode(line, typeText));
                    string opSymbol = isNegate ? "!is" : "is";
                    var op = new RelOpNode(line, opSymbol, (x, y) => isNegate ^ (x?.GetType() == y?.GetType()));
                    return new RelExprNode(line, left, op, right);
                }
                {
                    string inText = ctx.inOperator(0).GetText();
                    bool inNegate = inText.Contains("!");
                    ExprNode right = this.AsExprNode(this.Visit(ctx.elvisExpression(1)));
                    string opSymbol = inNegate ? "!in" : "in";
                    var op = new RelOpNode(line, opSymbol, (x, y) => inNegate ^ (x?.Equals(y) ?? false));
                    return new RelExprNode(line, left, op, right);
                }
            }
            return this.Visit(ctx.elvisExpression(0));
        }

        // Grammar rule: elvisExpression : infixFunctionCall (NL* ELVIS NL* infixFunctionCall)*
        /// <summary>
        /// Visits the elvis expression parse tree context.
        /// </summary>
        /// <param name="ctx">The elvis expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitElvisExpression(KotlinParser.ElvisExpressionContext ctx)
        {
            if (ctx.infixFunctionCall().Length == 1) return this.Visit(ctx.infixFunctionCall(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.infixFunctionCall(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.infixFunctionCall(1)));
            var op = new ArithmOpNode(line, "?:", (x, y) => x ?? y);
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: infixFunctionCall : rangeExpression (simpleIdentifier NL* rangeExpression)*
        /// <summary>
        /// Visits the infix function call parse tree context.
        /// </summary>
        /// <param name="ctx">The infix function call parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitInfixFunctionCall(KotlinParser.InfixFunctionCallContext ctx)
        {
            if (ctx.rangeExpression().Length == 1) return this.Visit(ctx.rangeExpression(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.rangeExpression(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.rangeExpression(1)));
            string infixName = ctx.simpleIdentifier(0).GetText();
            var op = new ArithmOpNode(line, infixName, (x, y) => null);
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: rangeExpression : additiveExpression (RANGE NL* additiveExpression)*
        /// <summary>
        /// Visits the range expression parse tree context.
        /// </summary>
        /// <param name="ctx">The range expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitRangeExpression(KotlinParser.RangeExpressionContext ctx)
        {
            if (ctx.additiveExpression().Length == 1) return this.Visit(ctx.additiveExpression(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.additiveExpression(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.additiveExpression(1)));
            var op = new ArithmOpNode(line, "..", (x, y) => null);
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: additiveExpression : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)*
        /// <summary>
        /// Visits the additive expression parse tree context.
        /// </summary>
        /// <param name="ctx">The additive expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitAdditiveExpression(KotlinParser.AdditiveExpressionContext ctx)
        {
            if (ctx.multiplicativeExpression().Length == 1) return this.Visit(ctx.multiplicativeExpression(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.multiplicativeExpression(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.multiplicativeExpression(1)));
            var op = ArithmOpNode.FromSymbol(line, ctx.additiveOperator(0).GetText());
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: multiplicativeExpression : typeRHS (multiplicativeOperation NL* typeRHS)*
        /// <summary>
        /// Visits the multiplicative expression parse tree context.
        /// </summary>
        /// <param name="ctx">The multiplicative expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitMultiplicativeExpression(KotlinParser.MultiplicativeExpressionContext ctx)
        {
            if (ctx.typeRHS().Length == 1) return this.Visit(ctx.typeRHS(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.typeRHS(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.typeRHS(1)));
            var op = ArithmOpNode.FromSymbol(line, ctx.multiplicativeOperation(0).GetText());
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: typeRHS : prefixUnaryExpression (NL* typeOperation prefixUnaryExpression)*
        /// <summary>
        /// Visits the type rhs parse tree context.
        /// </summary>
        /// <param name="ctx">The type rhs parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitTypeRHS(KotlinParser.TypeRHSContext ctx)
        {
            if (ctx.prefixUnaryExpression().Length == 1) return this.Visit(ctx.prefixUnaryExpression(0));
            int line = ctx.Start.Line;
            ExprNode left = this.AsExprNode(this.Visit(ctx.prefixUnaryExpression(0)));
            ExprNode right = this.AsExprNode(this.Visit(ctx.prefixUnaryExpression(1)));
            string opText = ctx.typeOperation(0).GetText();
            if (opText == "as" || opText == "as?")
            {
                bool safeCast = opText == "as?";
                string typeText = right is IdNode id ? id.Identifier : right.GetText();
                FuncCallExprNode castCall = this.MarkerExpression(line, safeCast ? "__linvast_safe_cast" : "__linvast_type_assert",
                    new IdNode(line, typeText));
                return new FuncCallExprNode(line, new IdNode(line, "__linvast_cast"),
                    new ExprListNode(line, left, castCall));
            }
            throw new NotImplementedException($"unsupported type operation: {opText}");
        }

        // Grammar rule: prefixUnaryExpression : prefixUnaryOperation* postfixUnaryExpression
        // TODO: support ++/-- prefix operators
        /// <summary>
        /// Visits the prefix unary expression parse tree context.
        /// </summary>
        /// <param name="ctx">The prefix unary expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPrefixUnaryExpression(KotlinParser.PrefixUnaryExpressionContext ctx)
        {
            if (ctx.prefixUnaryOperation().Length == 0) return this.Visit(ctx.postfixUnaryExpression());
            int line = ctx.Start.Line;
            ExprNode operand = this.AsExprNode(this.Visit(ctx.postfixUnaryExpression()));
            string op = ctx.prefixUnaryOperation(0).GetText();
            if (ctx.prefixUnaryOperation(0).labelDefinition() != null) return operand;
            if (ctx.prefixUnaryOperation(0).annotations() != null) return operand;
            return new UnaryExprNode(line, UnaryOpNode.FromSymbol(line, op), operand);
        }

        // Grammar rule: postfixUnaryExpression : (atomicExpression | callableReference) postfixUnaryOperation*
        /// <summary>
        /// Visits the postfix unary expression parse tree context.
        /// </summary>
        /// <param name="ctx">The postfix unary expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPostfixUnaryExpression(KotlinParser.PostfixUnaryExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ASTNode exprNode = ctx.atomicExpression() != null
                            ? this.Visit(ctx.atomicExpression())
                            : this.Visit(ctx.callableReference());

            if (ctx.postfixUnaryOperation().Length == 0) return exprNode;

            ExprNode expr = this.AsExprNode(exprNode);
            foreach (var op in ctx.postfixUnaryOperation())
            {
                if (op.INCR() != null)
                {
                    expr = this.MarkerExpression(line, "__linvast_postinc", expr);
                }
                else if (op.DECR() != null)
                {
                    expr = this.MarkerExpression(line, "__linvast_postdec", expr);
                }
                else if (op.EXCL().Length == 2)
                {
                    expr = this.MarkerExpression(line, "__linvast_assert_nonnull", expr);
                }
                else if (op.callSuffix() != null)
                {
                    expr = this.BuildCallExpression(expr, op.callSuffix());
                }
                else if (op.arrayAccess() != null)
                {
                    ExprListNode indices = this.VisitArrayAccessIndices(op.arrayAccess());
                    if (indices.Expressions.Count() == 1)
                    {
                        expr = new ArrAccessExprNode(line, expr, indices.Expressions.First());
                    }
                    else
                    {
                        expr = this.MarkerExpression(line, "__linvast_multiarray_access", expr, indices);
                    }
                }
                else if (op.memberAccessOperator() != null)
                {
                    ExprNode rhs = this.AsExprNode(this.Visit(op.postfixUnaryExpression()));
                    if (op.memberAccessOperator().DOT() != null)
                    {
                        expr = this.MergeMemberAccess(expr, rhs);
                    }
                    else
                    {
                        expr = this.MarkerExpression(line, "__linvast_nullsafe_access", expr, rhs);
                    }
                }
            }

            return expr;
        }

        private ExprNode BuildCallExpression(ExprNode receiver, KotlinParser.CallSuffixContext ctx)
        {
            int line = ctx.Start.Line;
            string funcName = receiver is IdNode id ? id.Identifier : "__linvast_invoke";

            if (ctx.valueArguments() != null)
            {
                ExprListNode args = this.BuildValueArguments(ctx.valueArguments());
                return new FuncCallExprNode(line, new IdNode(line, funcName), args);
            }

            return new FuncCallExprNode(line, new IdNode(line, funcName));
        }

        private ExprListNode BuildValueArguments(KotlinParser.ValueArgumentsContext ctx)
        {
            int line = ctx.Start.Line;
            var args = new List<ExprNode>();
            foreach (var argCtx in ctx.valueArgument())
            {
                if (argCtx.expression() != null)
                {
                    args.Add(this.AsExprNode(this.Visit(argCtx.expression())));
                }
            }
            return new ExprListNode(line, args);
        }

        private ExprListNode VisitArrayAccessIndices(KotlinParser.ArrayAccessContext ctx)
        {
            int line = ctx.Start.Line;
            var indices = new List<ExprNode>();
            foreach (var exprCtx in ctx.expression())
            {
                indices.Add(this.AsExprNode(this.Visit(exprCtx)));
            }
            return new ExprListNode(line, indices);
        }

        private ExprNode MergeMemberAccess(ExprNode receiver, ExprNode rhs)
        {
            int line = receiver.Line;
            if (receiver is IdNode recvId && rhs is IdNode rhsId)
            {
                return new IdNode(line, $"{recvId.Identifier}.{rhsId.Identifier}");
            }
            if (receiver is IdNode recvId2 && rhs is FuncCallExprNode funcCall)
            {
                string newId = $"{recvId2.Identifier}.{funcCall.Identifier}";
                return funcCall.Arguments is null
                    ? new FuncCallExprNode(line, new IdNode(line, newId))
                    : new FuncCallExprNode(line, new IdNode(line, newId), funcCall.Arguments);
            }
            return this.MarkerExpression(line, "__linvast_member_access", receiver, rhs);
        }

        // Grammar rule: atomicExpression : parenthesizedExpression | literalConstant | functionLiteral
        //                                 | thisExpression | superExpression | conditionalExpression
        //                                 | tryExpression | objectLiteral | jumpExpression
        //                                 | loopExpression | collectionLiteral | simpleIdentifier | VAL identifier
        /// <summary>
        /// Visits the atomic expression parse tree context.
        /// </summary>
        /// <param name="ctx">The atomic expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitAtomicExpression(KotlinParser.AtomicExpressionContext ctx)
        {
            if (ctx.literalConstant() != null) return this.Visit(ctx.literalConstant());
            if (ctx.simpleIdentifier() != null) return this.Visit(ctx.simpleIdentifier());
            if (ctx.parenthesizedExpression() != null) return this.Visit(ctx.parenthesizedExpression());
            if (ctx.conditionalExpression() != null) return this.Visit(ctx.conditionalExpression());
            if (ctx.jumpExpression() != null) return this.Visit(ctx.jumpExpression());
            if (ctx.loopExpression() != null) return this.Visit(ctx.loopExpression());
            if (ctx.tryExpression() != null) return this.Visit(ctx.tryExpression());
            if (ctx.functionLiteral() != null) return this.Visit(ctx.functionLiteral());
            if (ctx.thisExpression() != null) return new IdNode(ctx.Start.Line, "this");
            if (ctx.superExpression() != null) return new IdNode(ctx.Start.Line, "super");
            if (ctx.objectLiteral() != null) return new IdNode(ctx.Start.Line, "object");
            if (ctx.collectionLiteral() != null) return this.Visit(ctx.collectionLiteral());
            if (ctx.VAL() != null && ctx.identifier() != null) return new IdNode(ctx.Start.Line, ctx.identifier().GetText());
            throw new NotImplementedException("unsupported atomic expression");
        }

        // Grammar rule: literalConstant : BooleanLiteral | IntegerLiteral | HexLiteral | BinLiteral
        //                               | CharacterLiteral | RealLiteral | NullLiteral | LongLiteral | stringLiteral
        // TODO: support CharacterLiteral, LongLiteral, HexLiteral, BinLiteral
        /// <summary>
        /// Visits the literal constant parse tree context.
        /// </summary>
        /// <param name="ctx">The literal constant parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitLiteralConstant(KotlinParser.LiteralConstantContext ctx)
        {
            int line = ctx.Start.Line;
            if (ctx.BooleanLiteral() != null) return new LitExprNode(line, bool.Parse(ctx.BooleanLiteral().GetText()));
            if (ctx.IntegerLiteral() != null) return new LitExprNode(line, int.Parse(ctx.IntegerLiteral().GetText()));
            if (ctx.RealLiteral() != null) return new LitExprNode(line, double.Parse(ctx.RealLiteral().GetText(), CultureInfo.InvariantCulture));
            if (ctx.NullLiteral() != null) return new NullLitExprNode(line);
            if (ctx.stringLiteral() != null) return this.Visit(ctx.stringLiteral());
            if (ctx.CharacterLiteral() != null)
            {
                string raw = ctx.CharacterLiteral().GetText();
                return new LitExprNode(line, raw.Substring(1, raw.Length - 2));
            }
             if (ctx.LongLiteral() != null)
            {
                string raw = ctx.LongLiteral().GetText();
                string numPart = raw.EndsWith("L") ? raw.Substring(0, raw.Length - 1) : raw;
                return new LitExprNode(line, long.Parse(numPart));
            }
            if (ctx.HexLiteral() != null)
            {
                string raw = ctx.HexLiteral().GetText();
                string hexPart = raw.StartsWith("0x") || raw.StartsWith("0X") ? raw.Substring(2) : raw;
                hexPart = hexPart.Replace("_", "");
                return new LitExprNode(line, long.Parse(hexPart, NumberStyles.HexNumber, CultureInfo.InvariantCulture));
            }
            if (ctx.BinLiteral() != null)
            {
                string raw = ctx.BinLiteral().GetText();
                string binPart = raw.StartsWith("0b") || raw.StartsWith("0B") ? raw.Substring(2) : raw;
                binPart = binPart.Replace("_", "");
                return new LitExprNode(line, Convert.ToInt64(binPart, 2));
            }
            throw new NotImplementedException("unsupported literal type");
        }

        // Grammar rule: simpleIdentifier : Identifier | (many soft keywords)
        /// <summary>
        /// Visits the simple identifier parse tree context.
        /// </summary>
        /// <param name="ctx">The simple identifier parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSimpleIdentifier(KotlinParser.SimpleIdentifierContext ctx)
        {
            return new IdNode(ctx.Start.Line, ctx.GetText());
        }


        // Grammar rule: parenthesizedExpression : LPAREN expression RPAREN
        /// <summary>
        /// Visits the parenthesized expression parse tree context.
        /// </summary>
        /// <param name="ctx">The parenthesized expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParenthesizedExpression(KotlinParser.ParenthesizedExpressionContext ctx)
        {
            return this.Visit(ctx.expression());
        }

        // Grammar rule: stringLiteral : lineStringLiteral | multiLineStringLiteral
        /// <summary>
        /// Visits the string literal parse tree context.
        /// </summary>
        /// <param name="ctx">The string literal parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitStringLiteral(KotlinParser.StringLiteralContext ctx)
        {
            int line = ctx.Start.Line;
            if (ctx.lineStringLiteral() != null)
            {
                string raw = ctx.lineStringLiteral().GetText();
                string content = raw.Substring(1, raw.Length - 2);
                return new LitExprNode(line, content);
            }
            if (ctx.multiLineStringLiteral() != null)
            {
                string raw = ctx.multiLineStringLiteral().GetText();
                int startIdx = raw.IndexOf("\"\"\"");
                int endIdx = raw.LastIndexOf("\"\"\"");
                string content = raw.Substring(startIdx + 3, endIdx - startIdx - 3);
                return new LitExprNode(line, content);
            }
            throw new NotImplementedException("unsupported string literal");
        }

        // Grammar rule: callableReference : (COLONCOLON | Q_COLONCOLON) (simpleIdentifier | callableReference)
        //                                    | (COLONCOLON | Q_COLONCOLON) (CLASS | simpleIdentifier)
        /// <summary>
        /// Visits the callable reference parse tree context.
        /// </summary>
        /// <param name="ctx">The callable reference parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitCallableReference(KotlinParser.CallableReferenceContext ctx)
        {
            int line = ctx.Start.Line;
            if (ctx.identifier() != null && ctx.identifier().simpleIdentifier().Length > 0)
            {
                return new IdNode(line, "__linvast_callable_ref:" + ctx.identifier().simpleIdentifier(0).GetText());
            }
            if (ctx.CLASS() != null)
            {
                return new IdNode(line, "__linvast_callable_ref:" + ctx.CLASS().GetText());
            }
            if (ctx.userType() != null)
            {
                return new IdNode(line, "__linvast_callable_ref:" + ctx.userType().GetText());
            }
            return new IdNode(line, "__linvast_callable_ref:" + ctx.GetText());
        }

        // Grammar rule: functionLiteral : LCURL lambdaParameters? ARROW statements RCURL
        /// <summary>
        /// Visits the function literal parse tree context.        /// </summary>
        /// <param name="ctx">The function literal parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionLiteral(KotlinParser.FunctionLiteralContext ctx)
        {
            int line = ctx.Start.Line;
            BlockStatNode body = this.Visit(ctx.statements()).As<BlockStatNode>();
            return new LambdaFuncExprNode(line, body);
        }

        // Grammar rule: collectionLiteral : LSQUARE expression (COMMA expression)* COMMA? RSQUARE
        /// <summary>
        /// Visits the collection literal parse tree context.
        /// </summary>
        /// <param name="ctx">The collection literal parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitCollectionLiteral(KotlinParser.CollectionLiteralContext ctx)
        {
            int line = ctx.Start.Line;
            var exprs = new List<ExprNode>();
            foreach (var exprCtx in ctx.expression())
            {
                exprs.Add(this.AsExprNode(this.Visit(exprCtx)));
            }
            return new FuncCallExprNode(line, new IdNode(line, "__linvast_collection_literal"),
                new ExprListNode(line, exprs));
        }

        // Grammar rule: statements : statement* anysemi?
        /// <summary>
        /// Visits the statements parse tree context.
        /// </summary>
        /// <param name="ctx">The statements parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitStatements(KotlinParser.StatementsContext ctx)
        {
            int line = ctx.Start.Line;
            var children = ctx.statement().Select(s => this.Visit(s));
            return new BlockStatNode(line, children);
        }

        // Grammar rule: conditionalExpression : ifExpression | whenExpression
        /// <summary>
        /// Visits the conditional expression parse tree context.
        /// </summary>
        /// <param name="ctx">The conditional expression parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitConditionalExpression(KotlinParser.ConditionalExpressionContext ctx)
        {
            if (ctx.ifExpression() != null) return this.Visit(ctx.ifExpression());
            if (ctx.whenExpression() != null) return this.Visit(ctx.whenExpression());
            throw new NotImplementedException("unsupported conditional expression");
        }
    }
}
