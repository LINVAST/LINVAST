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
using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Logging;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Go
{
    [ASTBuilder(".go")]
    /// <summary>
    /// Builds a Go language AST from source code.
    /// </summary>

    public sealed partial class GoASTBuilder : GoParserBaseVisitor<ASTNode>, IASTBuilder<GoParser>
    {
        /// <summary>
        /// Creates an ANTLR parser for the given source code.
        /// </summary>
        /// <param name="code">The source code to parse.</param>
        /// <returns>An ANTLR parser configured for the source code.</returns>
        public ASTNode BuildFromSource(string code) => this.Visit(this.CreateParser(code).sourceFile());

        /// <summary>
        /// Creates an ANTLR parser for the given source code.
        /// </summary>
        /// <param name="code">The source code to parse.</param>
        /// <returns>An ANTLR parser configured for the source code.</returns>
        public GoParser CreateParser(string code) {
            ICharStream stream = CharStreams.fromstring(code);
            var lexer = new GoLexer(stream);
            lexer.AddErrorListener(new ThrowExceptionErrorListener());
            ITokenStream tokens = new CommonTokenStream(lexer);
            var parser = new GoParser(tokens);
            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowExceptionErrorListener());
            return parser;
        }

        /// <summary>
        /// Builds an AST from the given source code using a custom entry point.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <param name="entryProvider">A function that selects the entry point context from the parser.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code, Func<GoParser, ParserRuleContext> entryProvider) => 
            this.Visit(entryProvider(this.CreateParser(code)));
        
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
        /// Visits the source file parse tree context.
        /// </summary>
        /// <param name="ctx">The source file parse tree context.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitSourceFile(GoParser.SourceFileContext ctx)
        {
            var package = this.Visit(ctx.packageClause());
            var imports = ctx.importDecl().Select(this.Visit);
            var functions = ctx.functionDecl().Select(this.Visit);
            var methods = ctx.methodDecl().Select(this.Visit);
            var declarations = ctx.declaration().Select(this.Visit);
            return new SourceNode(new[] { package }.Concat(imports).Concat(functions).Concat(methods).Concat(declarations));
        }
    }
}
