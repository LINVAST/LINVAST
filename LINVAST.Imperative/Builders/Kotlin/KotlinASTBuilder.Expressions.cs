using System;
using System.Globalization;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: disjunction : conjunction (NL* DISJ NL* conjunction)*
        public override ASTNode VisitDisjunction(KotlinParser.DisjunctionContext ctx)
        {
            if (ctx.conjunction().Length == 1) return this.Visit(ctx.conjunction(0));
            int line = ctx.Start.Line;
            ExprNode left = this.Visit(ctx.conjunction(0)).As<ExprNode>();
            ExprNode right = this.Visit(ctx.conjunction(1)).As<ExprNode>();
            return new LogicExprNode(line, left, BinaryLogicOpNode.FromSymbol(line, "||"), right);
        }

        // Grammar rule: conjunction : equalityComparison (NL* CONJ NL* equalityComparison)*
        public override ASTNode VisitConjunction(KotlinParser.ConjunctionContext ctx)
        {
            if (ctx.equalityComparison().Length == 1) return this.Visit(ctx.equalityComparison(0));
            int line = ctx.Start.Line;
            ExprNode left = this.Visit(ctx.equalityComparison(0)).As<ExprNode>();
            ExprNode right = this.Visit(ctx.equalityComparison(1)).As<ExprNode>();
            return new LogicExprNode(line, left, BinaryLogicOpNode.FromSymbol(line, "&&"), right);
        }

        // Grammar rule: equalityComparison : comparison (equalityOperation NL* comparison)*
        public override ASTNode VisitEqualityComparison(KotlinParser.EqualityComparisonContext ctx)
        {
            if (ctx.comparison().Length == 1) return this.Visit(ctx.comparison(0));
            int line = ctx.Start.Line;
            ExprNode left = this.Visit(ctx.comparison(0)).As<ExprNode>();
            ExprNode right = this.Visit(ctx.comparison(1)).As<ExprNode>();
            var op = RelOpNode.FromSymbol(line, ctx.equalityOperation(0).GetText());
            return new RelExprNode(line, left, op, right);
        }

        // Grammar rule: comparison : namedInfix (comparisonOperator NL* namedInfix)?
        public override ASTNode VisitComparison(KotlinParser.ComparisonContext ctx)
        {
            if (ctx.namedInfix().Length == 1) return this.Visit(ctx.namedInfix(0));
            int line = ctx.Start.Line;
            ExprNode left = this.Visit(ctx.namedInfix(0)).As<ExprNode>();
            ExprNode right = this.Visit(ctx.namedInfix(1)).As<ExprNode>();
            var op = RelOpNode.FromSymbol(line, ctx.comparisonOperator().GetText());
            return new RelExprNode(line, left, op, right);
        }

        // Grammar rule: namedInfix : elvisExpression ((inOperator NL* elvisExpression)+ | (isOperator NL* type))?
        // TODO: support `is`, `!is`, `in`, `!in` operators
        public override ASTNode VisitNamedInfix(KotlinParser.NamedInfixContext ctx)
        {
            if (ctx.elvisExpression().Length == 1) return this.Visit(ctx.elvisExpression(0));
            throw new NotImplementedException();
        }

        // Grammar rule: elvisExpression : infixFunctionCall (NL* ELVIS NL* infixFunctionCall)*
        // TODO: support ?: (elvis operator)
        public override ASTNode VisitElvisExpression(KotlinParser.ElvisExpressionContext ctx)
        {
            if (ctx.infixFunctionCall().Length == 1) return this.Visit(ctx.infixFunctionCall(0));
            throw new NotImplementedException();
        }

        // Grammar rule: infixFunctionCall : rangeExpression (simpleIdentifier NL* rangeExpression)*
        // TODO: support infix function calls (e.g. `to`, `until`)
        public override ASTNode VisitInfixFunctionCall(KotlinParser.InfixFunctionCallContext ctx)
        {
            if (ctx.rangeExpression().Length == 1) return this.Visit(ctx.rangeExpression(0));
            throw new NotImplementedException();
        }

        // Grammar rule: rangeExpression : additiveExpression (RANGE NL* additiveExpression)*
        // TODO: support .. (range operator)
        public override ASTNode VisitRangeExpression(KotlinParser.RangeExpressionContext ctx)
        {
            if (ctx.additiveExpression().Length == 1) return this.Visit(ctx.additiveExpression(0));
            throw new NotImplementedException();
        }

        // Grammar rule: additiveExpression : multiplicativeExpression (additiveOperator NL* multiplicativeExpression)*
        public override ASTNode VisitAdditiveExpression(KotlinParser.AdditiveExpressionContext ctx)
        {
            if (ctx.multiplicativeExpression().Length == 1) return this.Visit(ctx.multiplicativeExpression(0));
            int line = ctx.Start.Line;
            ExprNode left = this.Visit(ctx.multiplicativeExpression(0)).As<ExprNode>();
            ExprNode right = this.Visit(ctx.multiplicativeExpression(1)).As<ExprNode>();
            var op = ArithmOpNode.FromSymbol(line, ctx.additiveOperator(0).GetText());
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: multiplicativeExpression : typeRHS (multiplicativeOperation NL* typeRHS)*
        public override ASTNode VisitMultiplicativeExpression(KotlinParser.MultiplicativeExpressionContext ctx)
        {
            if (ctx.typeRHS().Length == 1) return this.Visit(ctx.typeRHS(0));
            int line = ctx.Start.Line;
            ExprNode left = this.Visit(ctx.typeRHS(0)).As<ExprNode>();
            ExprNode right = this.Visit(ctx.typeRHS(1)).As<ExprNode>();
            var op = ArithmOpNode.FromSymbol(line, ctx.multiplicativeOperation(0).GetText());
            return new ArithmExprNode(line, left, op, right);
        }

        // Grammar rule: typeRHS : prefixUnaryExpression (NL* typeOperation prefixUnaryExpression)*
        // TODO: support `as`, `as?` type cast operations
        public override ASTNode VisitTypeRHS(KotlinParser.TypeRHSContext ctx)
        {
            if (ctx.prefixUnaryExpression().Length == 1) return this.Visit(ctx.prefixUnaryExpression(0));
            throw new NotImplementedException();
        }

        // Grammar rule: prefixUnaryExpression : prefixUnaryOperation* postfixUnaryExpression
        // TODO: support ++/-- prefix operators
        public override ASTNode VisitPrefixUnaryExpression(KotlinParser.PrefixUnaryExpressionContext ctx)
        {
            if (ctx.prefixUnaryOperation().Length == 0) return this.Visit(ctx.postfixUnaryExpression());
            int line = ctx.Start.Line;
            string op = ctx.prefixUnaryOperation(0).GetText();
            ExprNode operand = this.Visit(ctx.postfixUnaryExpression()).As<ExprNode>();
            return new UnaryExprNode(line, UnaryOpNode.FromSymbol(line, op), operand);
        }

        // Grammar rule: postfixUnaryExpression : (atomicExpression | callableReference) postfixUnaryOperation*
        // TODO: support postfix operations (++, --, member access, function calls)
        public override ASTNode VisitPostfixUnaryExpression(KotlinParser.PostfixUnaryExpressionContext ctx)
        {
            if (ctx.postfixUnaryOperation().Length == 0)
            {
                if (ctx.atomicExpression() != null) return this.Visit(ctx.atomicExpression());
                // TODO: support callableReference (::foo)
                throw new NotImplementedException("callableReference not supported");
            }
            throw new NotImplementedException();
        }

        // Grammar rule: atomicExpression : parenthesizedExpression | literalConstant | functionLiteral
        //                                 | thisExpression | superExpression | conditionalExpression
        //                                 | tryExpression | objectLiteral | jumpExpression
        //                                 | loopExpression | collectionLiteral | simpleIdentifier | VAL identifier
        // TODO: wire up functionLiteral, objectLiteral, thisExpression, superExpression, tryExpression, collectionLiteral, VAL identifier
        public override ASTNode VisitAtomicExpression(KotlinParser.AtomicExpressionContext ctx)
        {
            if (ctx.literalConstant() != null) return this.Visit(ctx.literalConstant());
            if (ctx.simpleIdentifier() != null) return this.Visit(ctx.simpleIdentifier());
            if (ctx.parenthesizedExpression() != null) return this.Visit(ctx.parenthesizedExpression());
            if (ctx.conditionalExpression() != null) return this.Visit(ctx.conditionalExpression());
            if (ctx.jumpExpression() != null) return this.Visit(ctx.jumpExpression());
            if (ctx.loopExpression() != null) return this.Visit(ctx.loopExpression());
            throw new NotImplementedException("unsupported atomic expression");
        }

        // Grammar rule: literalConstant : BooleanLiteral | IntegerLiteral | HexLiteral | BinLiteral
        //                               | CharacterLiteral | RealLiteral | NullLiteral | LongLiteral | stringLiteral
        // TODO: support CharacterLiteral, LongLiteral, HexLiteral, BinLiteral
        public override ASTNode VisitLiteralConstant(KotlinParser.LiteralConstantContext ctx)
        {
            int line = ctx.Start.Line;
            if (ctx.BooleanLiteral() != null) return new LitExprNode(line, bool.Parse(ctx.BooleanLiteral().GetText()));
            if (ctx.IntegerLiteral() != null) return new LitExprNode(line, int.Parse(ctx.IntegerLiteral().GetText()));
            if (ctx.RealLiteral() != null) return new LitExprNode(line, double.Parse(ctx.RealLiteral().GetText(), CultureInfo.InvariantCulture));
            if (ctx.NullLiteral() != null) return new NullLitExprNode(line);
            if (ctx.stringLiteral() != null) return this.Visit(ctx.stringLiteral());
            throw new NotImplementedException("unsupported literal type");
        }

        // Grammar rule: simpleIdentifier : Identifier | (many soft keywords)
        public override ASTNode VisitSimpleIdentifier(KotlinParser.SimpleIdentifierContext ctx)
        {
            return new IdNode(ctx.Start.Line, ctx.GetText());
        }


        // Grammar rule: parenthesizedExpression : LPAREN expression RPAREN
        public override ASTNode VisitParenthesizedExpression(KotlinParser.ParenthesizedExpressionContext ctx)
        {
            return this.Visit(ctx.expression());
        }
    }
}
