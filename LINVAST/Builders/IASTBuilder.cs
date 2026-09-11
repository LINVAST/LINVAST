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
using Antlr4.Runtime;
using LINVAST.Nodes;

namespace LINVAST.Builders
{
    /// <summary>Defines a contract for building an AST using a specific parser type.</summary>
    /// <typeparam name="TParser">The parser type used to parse the source code.</typeparam>
    public interface IASTBuilder<TParser> : IAbstractASTBuilder where TParser : Parser
    {
        /// <summary>Creates a new parser instance for the given source code.</summary>
        /// <param name="code">The source code to parse.</param>
        /// <returns>A new parser instance.</returns>
        TParser CreateParser(string code);
        /// <summary>Builds an AST from the source code using the specified entry rule.</summary>
        /// <param name="code">The source code to parse.</param>
        /// <param name="entryProvider">A function that selects the parser entry rule context.</param>
        /// <returns>The root AST node.</returns>
        ASTNode BuildFromSource(string code, Func<TParser, ParserRuleContext> entryProvider);
    }
}
