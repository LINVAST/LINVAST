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


﻿using System.Linq;
using Antlr4.Runtime;
using Serilog;
using Serilog.Events;

namespace LINVAST.Logging
{
    /// <summary>Provides logging utilities for parser rule contexts.</summary>
    public static class LogObj
    {
        /// <summary>Logs information about a parser rule context.</summary>
        /// <param name="ctx">The parser rule context to log.</param>
        /// <param name="level">The log event level.</param>
        public static void Context(ParserRuleContext ctx, LogEventLevel level = LogEventLevel.Debug)
        {
            Log.Write(
                level,
                "[{Depth}:{ContextType}] [{SourceInterval}] | children: {ChildrenCount} | {Code}",
                ctx.Depth(),
                ctx.GetType().Name,
                ctx.SourceInterval,
                ctx.ChildCount,
                ctx.GetText()
            );
        }

        /// <summary>Logs a visit event for a parser rule context.</summary>
        /// <param name="ctx">The parser rule context being visited, or <c>null</c>.</param>
        /// <param name="level">The log event level.</param>
        public static void Visit(ParserRuleContext? ctx, LogEventLevel level = LogEventLevel.Debug)
        {
            if (ctx is null)
                return;

            Log.Write(
                level,
                "Visiting [L{Line}:C{Column}:D{Depth}:{ContextType}] | children: {ChildrenCount} | {CodeInit} ...",
                ctx.Start.Line,
                ctx.Start.Column,
                ctx.Depth(),
                ctx.GetType().Name,
                ctx.ChildCount,
                string.Join(string.Empty, ctx.GetText().Take(30))
            );
        }
    }
}
