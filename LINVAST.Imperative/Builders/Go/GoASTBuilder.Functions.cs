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
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Go
{
    /// <summary>
    /// Builds a Go language AST from source code.
    /// </summary>

    public sealed partial class GoASTBuilder : GoParserBaseVisitor<ASTNode>, IASTBuilder<GoParser>
    {
        /// <summary>
        /// Visits the function decl parse tree context.
        /// </summary>
        /// <param name="context">The function decl parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitFunctionDecl(GoParser.FunctionDeclContext context)
        {
            var funcName = new IdNode(context.Start.Line, context.IDENTIFIER().GetText());
            FuncNode signature = this.Visit(context.signature()).As<FuncNode>();
            BlockStatNode? body = null;
            if (context.block() is not null) {
                body = this.Visit(context.block()).As<BlockStatNode>();
            }

            var declSpecs = (DeclSpecsNode)signature.ChildrenWithoutTags.ElementAt(0);

            if (signature.ParametersNode is null && body is null) {
                return new FuncNode(context.Start.Line, declSpecs,
                    new FuncDeclNode(context.Start.Line, funcName));
            }

            if (signature.ParametersNode is not null && body is not null) {
                return new FuncNode(context.Start.Line, declSpecs,
                    new FuncDeclNode(context.Start.Line, funcName, signature.ParametersNode, body));
            }

            if (signature.ParametersNode is not null) {
                return new FuncNode(context.Start.Line, declSpecs,
                    new FuncDeclNode(context.Start.Line, funcName, signature.ParametersNode));
            }

            if (body is not null) {
                return new FuncNode(context.Start.Line, declSpecs,
                    new FuncDeclNode(context.Start.Line, funcName, body));
            }

            throw new Exception("Unreachable code was reached!");
        }

        /// <summary>
        /// Visits the parameters parse tree context.
        /// </summary>
        /// <param name="context">The parameters parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParameters(GoParser.ParametersContext context)
        {
            var paramsArr = context.parameterDecl()
                .Select(p => this.Visit(p).As<FuncParamsNode>()).ToList();
            var funcParamsNode = new FuncParamsNode(context.Start.Line,
                paramsArr.SelectMany(p=>p.Parameters));
            if (paramsArr.Any() && paramsArr.Last().IsVariadic) {
                funcParamsNode.IsVariadic = true;
            }

            return funcParamsNode;
        }

        // note this returns FuncParamsNode, NOT FuncParamNode !
        /// <summary>
        /// Visits the parameter decl parse tree context.
        /// </summary>
        /// <param name="context">The parameter decl parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitParameterDecl(GoParser.ParameterDeclContext context)
        {
            TypeNameNode paramType = this.Visit(context.type_()).As<TypeNameNode>();
            var paramDeclSpec = new DeclSpecsNode(context.Start.Line, paramType);
            FuncParamNode[] @params;
            if (context.identifierList() is null) {
                @params = new FuncParamNode[]
                { new (context.Start.Line, paramDeclSpec, new VarDeclNode(context.Start.Line, 
                    new IdNode(context.Start.Line, "."))) };
            } else {
                IdListNode decls = this.Visit(context.identifierList()).As<IdListNode>();
                @params = decls.Identifiers
                    .Select(d => new FuncParamNode(context.Start.Line, paramDeclSpec, 
                        new VarDeclNode(context.Start.Line, d)))
                    .ToArray();
            }

            var funcParamsNode = new FuncParamsNode(context.Start.Line, @params);
            if (context.ELLIPSIS() is not null) {
                funcParamsNode.IsVariadic = true;
            }

            return funcParamsNode;
        }

        // this effectively returns a FuncNode for unnamed function without a body,
        // as there is no {return value, params} ASTNode
        // (not the greatest solution)
        /// <summary>
        /// Visits the signature parse tree context.
        /// </summary>
        /// <param name="context">The signature parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSignature(GoParser.SignatureContext context)
        {
            DeclSpecsNode? retTypeNode = null;
            GoParser.ResultContext? resultContext = context.result();
            if (resultContext is not null) {
                TypeNameNode retType = this.ResultTypeName(resultContext);
                retTypeNode = new DeclSpecsNode(context.Start.Line, retType);
            } else {
                retTypeNode = new DeclSpecsNode(context.Start.Line, "void");
            }

            FuncParamsNode @params = this.Visit(context.parameters()).As<FuncParamsNode>();
            var funcDecl = new FuncDeclNode(context.Start.Line, 
                new IdNode(context.Start.Line, "."), @params);

            return new FuncNode(context.Start.Line, retTypeNode, funcDecl);
        }
        
        # region method-specific stuff

        /// <summary>
        /// Visits the method decl parse tree context.
        /// </summary>
        /// <param name="context">The method decl parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitMethodDecl(GoParser.MethodDeclContext context)
        {
            FuncParamNode receiver = this.Visit(context.receiver()).As<FuncParamNode>();
            var funcName = new IdNode(context.Start.Line, receiver.Specifiers.TypeName + "." + context.IDENTIFIER().GetText());
            
            FuncNode signature = this.Visit(context.signature()).As<FuncNode>();
            FuncParamsNode parameters = this.PrependReceiverParameter(context.Start.Line, receiver, signature.ParametersNode);
            BlockStatNode? body = null;
            if (context.block() is not null) {
                body = this.Visit(context.block()).As<BlockStatNode>();
            }

            var declSpecs = (DeclSpecsNode)signature.ChildrenWithoutTags.ElementAt(0);

            if (!parameters.Parameters.Any() && body is null) {
                return new FuncNode(context.Start.Line, declSpecs,
                    new FuncDeclNode(context.Start.Line, funcName));
            }

            if (body is not null) {
                return new FuncNode(context.Start.Line, declSpecs,
                    new FuncDeclNode(context.Start.Line, funcName, parameters, body));
            }

            return new FuncNode(context.Start.Line, declSpecs,
                new FuncDeclNode(context.Start.Line, funcName, parameters));
        }

        /// <summary>
        /// Visits the receiver parse tree context.
        /// </summary>
        /// <param name="context">The receiver parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitReceiver(GoParser.ReceiverContext context)
        {
            FuncParamsNode receiver = this.Visit(context.parameters()).As<FuncParamsNode>();
            if (receiver.IsVariadic) {
                throw new NotSupportedException("Receiver type cannot be variadic!");
            }

            if (receiver.Parameters.Count() > 1) {
                throw new NotSupportedException("Receiver cannot have multiple params!");
            }

            return receiver.Parameters.Single();
        }

        private FuncParamsNode PrependReceiverParameter(int line, FuncParamNode receiver, FuncParamsNode? parameters)
        {
            FuncParamNode[] merged = new[] { receiver }
                .Concat(parameters?.Parameters ?? Enumerable.Empty<FuncParamNode>())
                .ToArray();
            var result = new FuncParamsNode(line, merged);
            result.IsVariadic = parameters?.IsVariadic ?? false;
            return result;
        }

        #endregion
    }
}
