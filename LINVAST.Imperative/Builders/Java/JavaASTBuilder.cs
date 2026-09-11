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
using Antlr4.Runtime.Misc;
using Antlr4.Runtime.Tree;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Logging;
using LINVAST.Nodes;
using static LINVAST.Imperative.Builders.Java.JavaParser;

namespace LINVAST.Imperative.Builders.Java
{
    [ASTBuilder(".java")]
    /// <summary>
    /// Builds a Java language AST from source code.
    /// </summary>

    public sealed partial class JavaASTBuilder : JavaBaseVisitor<ASTNode>, IASTBuilder<JavaParser>
    {
        /// <summary>
        /// Creates an ANTLR parser for the given source code.
        /// </summary>
        /// <param name="code">The source code to parse.</param>
        /// <returns>An ANTLR parser configured for the source code.</returns>
        public JavaParser CreateParser(string code)
        {
            ICharStream stream = CharStreams.fromstring(code);
            var lexer = new JavaLexer(stream);
            lexer.AddErrorListener(new ThrowExceptionErrorListener());
            ITokenStream tokens = new CommonTokenStream(lexer);
            var parser = new JavaParser(tokens);
            parser.BuildParseTree = true;
            parser.RemoveErrorListeners();
            parser.AddErrorListener(new ThrowExceptionErrorListener());
            return parser;
        }

        /// <summary>
        /// Builds an AST from the given source code.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code)
            => this.Visit(this.CreateParser(code).compilationUnit());

        /// <summary>
        /// Builds an AST from the given source code using a custom entry point.
        /// </summary>
        /// <param name="code">The source code to build the AST from.</param>
        /// <param name="entryProvider">A function that selects the entry point context from the parser.</param>
        /// <returns>The root AST node representing the parsed source.</returns>
        public ASTNode BuildFromSource(string code, Func<JavaParser, ParserRuleContext> entryProvider)
            => this.Visit(entryProvider(this.CreateParser(code)));

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
        /// Visits the compilation unit parse tree context.
        /// </summary>
        /// <param name="ctx">The compilation unit parse tree context. Must not be null.</param>
        /// <returns>The corresponding AST node.</returns>
        public override ASTNode VisitCompilationUnit([NotNull] CompilationUnitContext ctx)
        {
            if (ctx.packageDeclaration() is not null) {
                // TODO package declaration
            }

            var imports = ctx.importDeclaration().Select(this.Visit).ToList();
            var types = ctx.typeDeclaration().Select(this.Visit).ToList();
            return new SourceNode(imports.Concat(types));
        }
    }
}
