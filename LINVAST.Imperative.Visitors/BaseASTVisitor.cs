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


﻿using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Visitors
{
    /// <summary>
    /// Abstract base class for visitors that traverse an AST and produce a result of type <typeparamref name="TResult"/>.
    /// </summary>
    /// <typeparam name="TResult">The type of the result produced by visiting nodes.</typeparam>
    public abstract class BaseASTVisitor<TResult>
    {
        private const int MAX_ITER = 1000;
        private int repeatCount = 0;

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
        /// Dispatches the visit to the appropriate method based on the dynamic type of the node.
        /// Throws <see cref="EvaluationException"/> if the iteration limit is exceeded.
        /// </summary>
        /// <param name="node">The node to visit.</param>
        /// <returns>The result produced by the specific visitor method.</returns>
        /// <exception cref="LINVAST.Exceptions.EvaluationException">Thrown when the maximum iteration count is exceeded.</exception>
        public virtual TResult Visit(ASTNode node)
        {
            repeatCount++;
            if (repeatCount > MAX_ITER)
                throw new EvaluationException($"Evaluator reached {MAX_ITER} iterations. Perhaps a specialization for this node type does not exist?");
            return this.Visit((dynamic)node);
        }

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
        /// <summary>Visits a binary logic operator node by visiting its children.</summary>
        public virtual TResult Visit(BinaryLogicOpNode node) => this.VisitChildren(node);
        /// <summary>Visits a block statement node by visiting its children.</summary>
        public virtual TResult Visit(BlockStatNode node) => this.VisitChildren(node);
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
        /// <summary>Visits a null literal expression node by visiting its children.</summary>
        public virtual TResult Visit(NullLitExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a relational expression node by visiting its children.</summary>
        public virtual TResult Visit(RelExprNode node) => this.VisitChildren(node);
        /// <summary>Visits a relational operator node by visiting its children.</summary>
        public virtual TResult Visit(RelOpNode node) => this.VisitChildren(node);
        /// <summary>Visits a switch statement node by visiting its children.</summary>
        public virtual TResult Visit(SwitchStatNode node) => this.VisitChildren(node);
        /// <summary>Visits a throw statement node by visiting its children.</summary>
        public virtual TResult Visit(ThrowStatNode node) => this.VisitChildren(node);
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
