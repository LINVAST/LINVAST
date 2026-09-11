// LINVAST - Language-INVariant AST library
// Copyright (C) 2026 Ivan Ristović
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

﻿using System;
using System.Linq;
using Antlr4.Runtime.Misc;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.C.CParser;

namespace LINVAST.Imperative.Builders.C
{
    /// <summary>
    /// Builds a C language AST from source code.
    /// </summary>

    public sealed partial class CASTBuilder : CBaseVisitor<ASTNode>, IASTBuilder<CParser>
    {
        /// <summary>
        /// Visits the expression parse tree context.
        /// </summary>
        /// <param name="ctx">The expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitExpression([NotNull] ExpressionContext ctx)
        {
            ExprNode expr = this.Visit(ctx.assignmentExpression()).As<ExprNode>();
            if (ctx.expression() is not null) {
                ExprNode left = this.Visit(ctx.expression()).As<ExprNode>();
                return left is ExprListNode list
                    ? new ExprListNode(ctx.Start.Line, list.Expressions.Concat(new[] { expr }))
                    : new ExprListNode(ctx.Start.Line, left, expr);
            }

            return expr;
        }

        /// <summary>
        /// Visits the assignment expression parse tree context.
        /// </summary>
        /// <param name="ctx">The assignment expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitAssignmentExpression([NotNull] AssignmentExpressionContext ctx)
        {
            if (ctx.DigitSequence() is not null)
                return LitExprNode.FromString(ctx.Start.Line, ctx.DigitSequence().GetText());

            if (ctx.conditionalExpression() is not null)
                return this.Visit(ctx.conditionalExpression());

            ExprNode unary = this.Visit(ctx.unaryExpression()).As<ExprNode>();
            string symbol = ctx.children[1].GetText();
            var op = AssignOpNode.FromSymbol(ctx.Start.Line, symbol);
            ExprNode expr = this.Visit(ctx.assignmentExpression()).As<ExprNode>();

            return new AssignExprNode(ctx.Start.Line, unary, op, expr);
        }

        /// <summary>
        /// Visits the conditional expression parse tree context.
        /// </summary>
        /// <param name="ctx">The conditional expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitConditionalExpression([NotNull] ConditionalExpressionContext ctx)
        {
            ExprNode expr = this.Visit(ctx.logicalOrExpression()).As<ExprNode>();
            if (ctx.expression() is null)
                return expr;

            ExprNode thenExpr = this.Visit(ctx.expression()).As<ExprNode>();
            ExprNode elseExpr = this.Visit(ctx.conditionalExpression()).As<ExprNode>();
            return new CondExprNode(ctx.Start.Line, expr, thenExpr, elseExpr);
        }

        /// <summary>
        /// Visits the constant expression parse tree context.
        /// </summary>
        /// <param name="ctx">The constant expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitConstantExpression([NotNull] ConstantExpressionContext ctx)
            => this.Visit(ctx.conditionalExpression());

        /// <summary>
        /// Visits the logical or expression parse tree context.
        /// </summary>
        /// <param name="ctx">The logical or expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitLogicalOrExpression([NotNull] LogicalOrExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.logicalOrExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.logicalAndExpression()).As<ExprNode>();
                var op = BinaryLogicOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
                return new LogicExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.logicalAndExpression());
            }
        }

        /// <summary>
        /// Visits the logical and expression parse tree context.
        /// </summary>
        /// <param name="ctx">The logical and expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitLogicalAndExpression([NotNull] LogicalAndExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.logicalAndExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.inclusiveOrExpression()).As<ExprNode>();
                var op = BinaryLogicOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
                return new LogicExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.inclusiveOrExpression());
            }
        }

        /// <summary>
        /// Visits the inclusive or expression parse tree context.
        /// </summary>
        /// <param name="ctx">The inclusive or expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitInclusiveOrExpression([NotNull] InclusiveOrExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.inclusiveOrExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.exclusiveOrExpression()).As<ExprNode>();
                string symbol = ctx.children[1].GetText();
                var op = new ArithmOpNode(ctx.Start.Line, symbol, BinaryOperations.BitwiseOrPrimitive);
                return new ArithmExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.exclusiveOrExpression());
            }
        }

        /// <summary>
        /// Visits the exclusive or expression parse tree context.
        /// </summary>
        /// <param name="ctx">The exclusive or expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitExclusiveOrExpression([NotNull] ExclusiveOrExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.exclusiveOrExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.andExpression()).As<ExprNode>();
                string symbol = ctx.children[1].GetText();
                var op = new ArithmOpNode(ctx.Start.Line, symbol, BinaryOperations.BitwiseXorPrimitive);
                return new ArithmExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.andExpression());
            }
        }

        /// <summary>
        /// Visits the and expression parse tree context.
        /// </summary>
        /// <param name="ctx">The and expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitAndExpression([NotNull] AndExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.andExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.equalityExpression()).As<ExprNode>();
                string symbol = ctx.children[1].GetText();
                var op = new ArithmOpNode(ctx.Start.Line, symbol, BinaryOperations.BitwiseAndPrimitive);
                return new ArithmExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.equalityExpression());
            }
        }

        /// <summary>
        /// Visits the equality expression parse tree context.
        /// </summary>
        /// <param name="ctx">The equality expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitEqualityExpression([NotNull] EqualityExpressionContext ctx)
        {
            if (ctx.equalityExpression() is null)
                return this.Visit(ctx.relationalExpression());

            ExprNode left = this.Visit(ctx.equalityExpression()).As<ExprNode>();
            ExprNode right = this.Visit(ctx.relationalExpression()).As<ExprNode>();
            var op = RelOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
            return new RelExprNode(ctx.Start.Line, left, op, right);
        }

        /// <summary>
        /// Visits the relational expression parse tree context.
        /// </summary>
        /// <param name="ctx">The relational expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitRelationalExpression([NotNull] RelationalExpressionContext ctx)
        {
            if (ctx.relationalExpression() is null)
                return this.Visit(ctx.shiftExpression());

            ExprNode left = this.Visit(ctx.relationalExpression()).As<ExprNode>();
            ExprNode right = this.Visit(ctx.shiftExpression()).As<ExprNode>();
            var op = RelOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
            return new RelExprNode(ctx.Start.Line, left, op, right);
        }

        /// <summary>
        /// Visits the shift expression parse tree context.
        /// </summary>
        /// <param name="ctx">The shift expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitShiftExpression([NotNull] ShiftExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.shiftExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.additiveExpression()).As<ExprNode>();
                var op = ArithmOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
                return new ArithmExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.additiveExpression());
            }
        }

        /// <summary>
        /// Visits the additive expression parse tree context.
        /// </summary>
        /// <param name="ctx">The additive expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitAdditiveExpression([NotNull] AdditiveExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.additiveExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.multiplicativeExpression()).As<ExprNode>();
                var op = ArithmOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
                return new ArithmExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.multiplicativeExpression());
            }
        }

        /// <summary>
        /// Visits the multiplicative expression parse tree context.
        /// </summary>
        /// <param name="ctx">The multiplicative expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitMultiplicativeExpression([NotNull] MultiplicativeExpressionContext ctx)
        {
            if (ctx.ChildCount > 1) {
                ExprNode left = this.Visit(ctx.multiplicativeExpression()).As<ExprNode>();
                ExprNode right = this.Visit(ctx.castExpression()).As<ExprNode>();
                var op = ArithmOpNode.FromSymbol(ctx.Start.Line, ctx.children[1].GetText());
                return new ArithmExprNode(ctx.Start.Line, left, op, right);
            } else {
                return this.Visit(ctx.castExpression());
            }
        }

        /// <summary>
        /// Visits the cast expression parse tree context.
        /// </summary>
        /// <param name="ctx">The cast expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitCastExpression([NotNull] CastExpressionContext ctx)
        {
            if (ctx.DigitSequence() is not null)
                return LitExprNode.FromString(ctx.Start.Line, ctx.DigitSequence().GetText());

            return ctx.unaryExpression() is not null
                ? this.Visit(ctx.unaryExpression())
                : this.Visit(ctx.castExpression());
        }

        /// <summary>
        /// Visits the unary expression parse tree context.
        /// </summary>
        /// <param name="ctx">The unary expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitUnaryExpression([NotNull] UnaryExpressionContext ctx)
        {
            if (ctx.postfixExpression() is not null)
                return this.Visit(ctx.postfixExpression());

            ExprNode expr;
            if (ctx.unaryExpression() is null) {
                if (ctx.typeName() is not null) {
                    string callee = ctx.children.First().GetText();
                    var typeName = new IdNode(ctx.typeName().Start.Line, ctx.typeName().GetText());
                    return new FuncCallExprNode(ctx.Start.Line, new IdNode(ctx.Start.Line, callee), new ExprListNode(ctx.Start.Line, typeName));
                }

                if (ctx.Identifier() is not null)
                    return new IdNode(ctx.Start.Line, $"&&{ctx.Identifier().GetText()}");

                if (ctx.castExpression() is null)
                    throw new NotImplementedException("extended unary expressions");
                expr = this.Visit(ctx.castExpression()).As<ExprNode>();
            } else if (ctx.GetToken(Sizeof, 0) is not null) {
                expr = this.Visit(ctx.unaryExpression()).As<ExprNode>();
                return new FuncCallExprNode(ctx.Start.Line, new IdNode(ctx.Start.Line, "sizeof"), new ExprListNode(ctx.Start.Line, expr));
            } else {
                expr = this.Visit(ctx.unaryExpression()).As<ExprNode>();
            }
            var op = UnaryOpNode.FromSymbol(ctx.Start.Line, ctx.children[0].GetText());
            return new UnaryExprNode(ctx.Start.Line, op, expr);
        }

        /// <summary>
        /// Visits the postfix expression parse tree context.
        /// </summary>
        /// <param name="ctx">The postfix expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPostfixExpression([NotNull] PostfixExpressionContext ctx)
        {
            if (ctx.primaryExpression() is not null)
                return this.Visit(ctx.primaryExpression());

            if (ctx.typeName() is not null) {
                var typeName = new IdNode(ctx.typeName().Start.Line, ctx.typeName().GetText());
                if (ctx.initializerList() is not null) {
                    ArrInitExprNode initializers = this.Visit(ctx.initializerList()).As<ArrInitExprNode>();
                    return new ConsExprNode(ctx.Start.Line, typeName, new ExprListNode(initializers.Line, initializers.Initializers));
                }

                return new ConsExprNode(ctx.Start.Line, typeName);
            }

            if (ctx.initializerList() is not null)
                return this.Visit(ctx.initializerList()).As<ExprNode>();

            ExprNode expr = this.Visit(ctx.postfixExpression()).As<ExprNode>();
            switch (ctx.children[1].GetText()) {
                case "(":
                    IdNode fname = expr is IdNode id ? id : new IdNode(expr.Line, expr.GetText());
                    if (ctx.argumentExpressionList() is not null) {
                        ExprListNode? args = this.Visit(ctx.argumentExpressionList()).As<ExprListNode>();
                        return new FuncCallExprNode(ctx.Start.Line, fname, args);
                    } else {
                        return new FuncCallExprNode(ctx.Start.Line, fname);
                    }
                case "[":
                    ExprNode indexExpr = this.Visit(ctx.expression()).As<ExprNode>();
                    return new ArrAccessExprNode(ctx.Start.Line, expr, indexExpr);
                case "++":
                    return new IncExprNode(ctx.Start.Line, expr);
                case "--":
                    return new DecExprNode(ctx.Start.Line, expr);
                case "->":
                case ".":
                    return new IdNode(ctx.Start.Line, $"{expr.GetText()}{ctx.children[1].GetText()}{ctx.Identifier().GetText()}");
                default:
                    throw new SyntaxErrorException("Unknown postfix expression", ctx.Start.Line, ctx.Start.Column);
            }
        }

        /// <summary>
        /// Visits the primary expression parse tree context.
        /// </summary>
        /// <param name="ctx">The primary expression parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitPrimaryExpression([NotNull] PrimaryExpressionContext ctx)
        {
            if (ctx.Identifier() is not null) {
                string name = ctx.Identifier().GetText();
                if (name.Equals("null", StringComparison.InvariantCultureIgnoreCase))
                    return new NullLitExprNode(ctx.Start.Line);
                return new IdNode(ctx.Start.Line, name);
            }

            if (ctx.Constant() is not null)
                return LitExprNode.FromString(ctx.Start.Line, ctx.Constant().GetText());

            if (ctx.expression() is not null)
                return this.Visit(ctx.expression());

            if (ctx.StringLiteral() is not null)
                return new LitExprNode(ctx.Start.Line, string.Join("", ctx.StringLiteral().Select(t => t.GetText()[1..^1])));

            if (ctx.genericSelection() is not null)
                return this.Visit(ctx.genericSelection());

            if (ctx.unaryExpression() is not null) {
                ExprNode value = this.Visit(ctx.unaryExpression()).As<ExprNode>();
                ExprNode type = new IdNode(ctx.typeName().Start.Line, ctx.typeName().GetText());
                string builtin = ctx.Start.Text ?? "__builtin_va_arg";
                ExprListNode args = builtin == "__builtin_offsetof"
                    ? new ExprListNode(ctx.Start.Line, type, value)
                    : new ExprListNode(ctx.Start.Line, value, type);
                return new FuncCallExprNode(ctx.Start.Line, new IdNode(ctx.Start.Line, builtin), args);
            }

            if (ctx.typeName() is not null) {
                ExprNode type = new IdNode(ctx.typeName().Start.Line, ctx.typeName().GetText());
                return new FuncCallExprNode(ctx.Start.Line, new IdNode(ctx.Start.Line, "__builtin_offsetof"), new ExprListNode(ctx.Start.Line, type));
            }

            if (ctx.compoundStatement() is not null)
                return new IdNode(ctx.Start.Line, this.Visit(ctx.compoundStatement()).GetText());

            throw new NotImplementedException("primary expression");
        }

        /// <summary>
        /// Visits the generic selection parse tree context.
        /// </summary>
        /// <param name="ctx">The generic selection parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitGenericSelection([NotNull] GenericSelectionContext ctx)
            => this.Visit(ctx.genericAssocList());

        /// <summary>
        /// Visits the generic assoc list parse tree context.
        /// </summary>
        /// <param name="ctx">The generic assoc list parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitGenericAssocList([NotNull] GenericAssocListContext ctx)
        {
            ExprNode expr = this.Visit(ctx.genericAssociation()).As<ExprNode>();

            if (ctx.genericAssocList() is null)
                return expr;

            ExprNode listExpr = this.Visit(ctx.genericAssocList()).As<ExprNode>();
            return IsDefaultGenericAssociation(ctx.genericAssociation()) ? expr : listExpr;
        }

        /// <summary>
        /// Visits the generic association parse tree context.
        /// </summary>
        /// <param name="ctx">The generic association parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitGenericAssociation([NotNull] GenericAssociationContext ctx)
            => this.Visit(ctx.assignmentExpression());

        /// <summary>
        /// Visits the argument expression list parse tree context.
        /// </summary>
        /// <param name="ctx">The argument expression list parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitArgumentExpressionList([NotNull] ArgumentExpressionListContext ctx)
        {
            ExprListNode args;
            ExprNode arg = this.Visit(ctx.assignmentExpression()).As<ExprNode>();

            if (ctx.argumentExpressionList() is null)
                return new ExprListNode(ctx.Start.Line, arg);

            args = this.Visit(ctx.argumentExpressionList()).As<ExprListNode>();
            arg.Parent = args;
            return new ExprListNode(ctx.Start.Line, args.Expressions.Concat(new[] { arg }));
        }

        private static bool IsDefaultGenericAssociation([NotNull] GenericAssociationContext ctx)
            => ctx.typeName() is null;
    }
}
