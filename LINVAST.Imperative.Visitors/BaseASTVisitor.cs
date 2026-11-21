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


﻿using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Visitors
{
    /// <summary>
    /// Abstract base class for visitors that traverse an AST and produce a result of type <typeparamref name="TResult"/>.
    /// </summary>
    /// <typeparam name="TResult">The type of the result produced by visiting nodes.</typeparam>
    public abstract class BaseASTVisitor<TResult>
    {
        /// <summary>
        /// Visits all children of the specified node, aggregating their results.
        /// </summary>
        /// <param name="node">The node whose children will be visited.</param>
        /// <returns>The aggregated result of visiting all children.</returns>
        public virtual TResult VisitChildren(ASTNode node)
        {
            TResult result = this.DefaultResult;
            foreach (ASTNode child in node.Children) {
                if (!this.ShouldVisitNextChild(node, result))
                    break;
                TResult childResult = this.Visit(child);
                result = this.AggregateResult(result, childResult);
            }
            return result;
        }

        /// <summary>
        /// Dispatches to a typed visitor method, or visits children for an unrecognized node type.
        /// Traversal has no cumulative node limit, so a visitor can be reused across trees.
        /// </summary>
        /// <param name="node">The node to visit.</param>
        /// <returns>The result produced by the specific visitor method or the child traversal.</returns>
        public virtual TResult Visit(ASTNode node) => node switch
        {
            CastExprNode typed => this.Visit(typed),
            DecExprNode typed => this.Visit(typed),
            ForStatNode typed => this.Visit(typed),
            FuncNode typed => this.Visit(typed),
            IncExprNode typed => this.Visit(typed),
            ThrowStatNode typed => this.Visit(typed),
            WhileStatNode typed => this.Visit(typed),
            ArithmExprNode typed => this.Visit(typed),
            ArithmOpNode typed => this.Visit(typed),
            ArrDeclNode typed => this.Visit(typed),
            ArrInitExprNode typed => this.Visit(typed),
            AssignExprNode typed => this.Visit(typed),
            AssignOpNode typed => this.Visit(typed),
            AsyncStatNode typed => this.Visit(typed),
            BinaryLogicOpNode typed => this.Visit(typed),
            BlockStatNode typed => this.Visit(typed),
            DeclStatNode typed => this.Visit(typed),
            DictDeclNode typed => this.Visit(typed),
            DictInitNode typed => this.Visit(typed),
            DeleteStatNode typed => this.Visit(typed),
            ExprStatNode typed => this.Visit(typed),
            ForeachStatNode typed => this.Visit(typed),
            FuncDeclNode typed => this.Visit(typed),
            IdListNode typed => this.Visit(typed),
            GlobalStatNode typed => this.Visit(typed),
            IfStatNode typed => this.Visit(typed),
            JumpStatNode typed => this.Visit(typed),
            LabeledStatNode typed => this.Visit(typed),
            LogicExprNode typed => this.Visit(typed),
            NonlocalStatNode typed => this.Visit(typed),
            NullLitExprNode typed => this.Visit(typed),
            RelExprNode typed => this.Visit(typed),
            RelOpNode typed => this.Visit(typed),
            SwitchStatNode typed => this.Visit(typed),
            TryStatNode typed => this.Visit(typed),
            VarDeclNode typed => this.Visit(typed),
            WithStatNode typed => this.Visit(typed),
            ArrAccessExprNode typed => this.Visit(typed),
            CondExprNode typed => this.Visit(typed),
            DeclSpecsNode typed => this.Visit(typed),
            DeclListNode typed => this.Visit(typed),
            DictEntryNode typed => this.Visit(typed),
            EmptyStatNode typed => this.Visit(typed),
            ExprListNode typed => this.Visit(typed),
            FuncCallExprNode typed => this.Visit(typed),
            FuncParamNode typed => this.Visit(typed),
            FuncParamsNode typed => this.Visit(typed),
            IdNode typed => this.Visit(typed),
            LambdaFuncExprNode typed => this.Visit(typed),
            LitExprNode typed => this.Visit(typed),
            UnaryExprNode typed => this.Visit(typed),
            UnaryOpNode typed => this.Visit(typed),
            YieldExprNode typed => this.Visit(typed),
            CatchClauseNode typed => this.Visit(typed),
            TagNode typed => this.Visit(typed),
            SourceNode typed => this.Visit(typed),
            _ => this.VisitChildren(node),
        };

        public virtual TResult Visit(CastExprNode node) => this.VisitChildren(node);

        /// <summary>Visits an arithmetic expression node by visiting its children.</summary>
        public virtual TResult Visit(ArithmExprNode node) => this.VisitChildren(node);
        /// <summary>Visits an arithmetic operator node by visiting its children.</summary>
        public virtual TResult Visit(ArithmOpNode node) => this.VisitChildren(node);
        /// <summary>Visits an array access expression node by visiting its children.</summary>
        public virtual TResult Visit(ArrAccessExprNode node) => this.VisitChildren(node);
        /// <summary>Visits an array declaration node by visiting its children.</summary>
        public virtual TResult Visit(ArrDeclNode node) => this.VisitChildren(node);
        /// <summary>Visits an array initialization expression node by visiting its children.</summary>
        public virtual TResult Visit(ArrInitExprNode node) => this.VisitChildren(node);
        /// <summary>Visits an assignment expression node by visiting its children.</summary>
        public virtual TResult Visit(AssignExprNode node) => this.VisitChildren(node);
        /// <summary>Visits an assignment operator node by visiting its children.</summary>
        public virtual TResult Visit(AssignOpNode node) => this.VisitChildren(node);
        /// <summary>Visits an async statement node by visiting its children.</summary>
        public virtual TResult Visit(AsyncStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a binary logic operator node by visiting its children.</summary>
        public virtual TResult Visit(BinaryLogicOpNode node) => this.VisitChildren(node);
        /// <summary>Visits a block statement node by visiting its children.</summary>
        public virtual TResult Visit(BlockStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a catch clause node by visiting its children.</summary>
        public virtual TResult Visit(CatchClauseNode node) => this.VisitChildren(node);
        /// <summary>Visits a conditional expression node by visiting its children.</summary>
        public virtual TResult Visit(CondExprNode node) => this.VisitChildren(node);
        /// <summary>Visits declaration specifications node by visiting its children.</summary>
        public virtual TResult Visit(DeclSpecsNode node) => this.VisitChildren(node);
        /// <summary>Visits a declaration statement node by visiting its children.</summary>
        public virtual TResult Visit(DeclStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a declaration list node by visiting its children.</summary>
        public virtual TResult Visit(DeclListNode node) => this.VisitChildren(node);
        /// <summary>Visits a decrement expression node by visiting its children.</summary>
        public virtual TResult Visit(DecExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a dictionary declaration node by visiting its children.</summary>
        public virtual TResult Visit(DictDeclNode node) => this.VisitChildren(node);
        /// <summary>Visits a dictionary entry node by visiting its children.</summary>
        public virtual TResult Visit(DictEntryNode node) => this.VisitChildren(node);
        /// <summary>Visits a dictionary initialization node by visiting its children.</summary>
        public virtual TResult Visit(DictInitNode node) => this.VisitChildren(node);
        /// <summary>Visits a delete statement node by visiting its children.</summary>
        public virtual TResult Visit(DeleteStatNode node) => this.VisitChildren(node);
        /// <summary>Visits an empty statement node by visiting its children.</summary>
        public virtual TResult Visit(EmptyStatNode node) => this.VisitChildren(node);
        /// <summary>Visits an expression list node by visiting its children.</summary>
        public virtual TResult Visit(ExprListNode node) => this.VisitChildren(node);
        /// <summary>Visits an expression statement node by visiting its children.</summary>
        public virtual TResult Visit(ExprStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a foreach statement node by visiting its children.</summary>
        public virtual TResult Visit(ForeachStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a for statement node by visiting its children.</summary>
        public virtual TResult Visit(ForStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a function call expression node by visiting its children.</summary>
        public virtual TResult Visit(FuncCallExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a function declaration node by visiting its children.</summary>
        public virtual TResult Visit(FuncDeclNode node) => this.VisitChildren(node);
        /// <summary>Visits a function node by visiting its children.</summary>
        public virtual TResult Visit(FuncNode node) => this.VisitChildren(node);
        /// <summary>Visits a function parameter node by visiting its children.</summary>
        public virtual TResult Visit(FuncParamNode node) => this.VisitChildren(node);
        /// <summary>Visits a function parameters node by visiting its children.</summary>
        public virtual TResult Visit(FuncParamsNode node) => this.VisitChildren(node);
        /// <summary>Visits an identifier node by visiting its children.</summary>
        public virtual TResult Visit(IdNode node) => this.VisitChildren(node);
        /// <summary>Visits an identifier list node by visiting its children.</summary>
        public virtual TResult Visit(IdListNode node) => this.VisitChildren(node);
        /// <summary>Visits a global statement node by visiting its children.</summary>
        public virtual TResult Visit(GlobalStatNode node) => this.VisitChildren(node);
        /// <summary>Visits an if statement node by visiting its children.</summary>
        public virtual TResult Visit(IfStatNode node) => this.VisitChildren(node);
        /// <summary>Visits an increment expression node by visiting its children.</summary>
        public virtual TResult Visit(IncExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a jump statement node by visiting its children.</summary>
        public virtual TResult Visit(JumpStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a labeled statement node by visiting its children.</summary>
        public virtual TResult Visit(LabeledStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a lambda function expression node by visiting its children.</summary>
        public virtual TResult Visit(LambdaFuncExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a literal expression node by visiting its children.</summary>
        public virtual TResult Visit(LitExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a logic expression node by visiting its children.</summary>
        public virtual TResult Visit(LogicExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a nonlocal statement node by visiting its children.</summary>
        public virtual TResult Visit(NonlocalStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a null literal expression node by visiting its children.</summary>
        public virtual TResult Visit(NullLitExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a relational expression node by visiting its children.</summary>
        public virtual TResult Visit(RelExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a relational operator node by visiting its children.</summary>
        public virtual TResult Visit(RelOpNode node) => this.VisitChildren(node);
        /// <summary>Visits a switch statement node by visiting its children.</summary>
        public virtual TResult Visit(SwitchStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a tag node by visiting its children.</summary>
        public virtual TResult Visit(TagNode node) => this.VisitChildren(node);
        /// <summary>Visits a throw statement node by visiting its children.</summary>
        public virtual TResult Visit(ThrowStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a try statement node by visiting its children.</summary>
        public virtual TResult Visit(TryStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a source node by visiting its children.</summary>
        public virtual TResult Visit(SourceNode node) => this.VisitChildren(node);
        /// <summary>Visits a unary expression node by visiting its children.</summary>
        public virtual TResult Visit(UnaryExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a unary operator node by visiting its children.</summary>
        public virtual TResult Visit(UnaryOpNode node) => this.VisitChildren(node);
        /// <summary>Visits a variable declaration node by visiting its children.</summary>
        public virtual TResult Visit(VarDeclNode node) => this.VisitChildren(node);
        /// <summary>Visits a while statement node by visiting its children.</summary>
        public virtual TResult Visit(WhileStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a with statement node by visiting its children.</summary>
        public virtual TResult Visit(WithStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a yield expression node by visiting its children.</summary>
        public virtual TResult Visit(YieldExprNode node) => this.VisitChildren(node);

        /// <summary>
        /// Gets the default result value used when aggregating child results.
        /// </summary>
        protected virtual TResult DefaultResult => default!;
        /// <summary>
        /// Aggregates the result of visiting a child with the accumulated result.
        /// </summary>
        /// <param name="aggregate">The accumulated result so far.</param>
        /// <param name="nextResult">The result from the next child.</param>
        /// <returns>The new aggregated result.</returns>
        protected virtual TResult AggregateResult(TResult aggregate, TResult nextResult) => nextResult;
        /// <summary>
        /// Determines whether the next child of a node should be visited.
        /// </summary>
        /// <param name="node">The node whose children are being visited.</param>
        /// <param name="currentResult">The current aggregated result.</param>
        /// <returns>True to continue visiting children; otherwise false.</returns>
        protected virtual bool ShouldVisitNextChild(ASTNode node, TResult currentResult) => true;
    }
}
