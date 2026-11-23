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

using System;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Builders.Go;
using LINVAST.Imperative.Builders.Java;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Imperative.Builders.Lua;
using LINVAST.Imperative.Builders.Python;

namespace LINVAST.Imperative.Builders.Common
{
    /// <summary>
    /// Factory that returns the <see cref="IExpressionSemantics"/> implementation for a
    /// given source language.
    /// </summary>
    public static class ExpressionSemanticsFactory
    {
        /// <summary>
        /// Returns the expression-semantics policy for the given language.
        /// </summary>
        /// <param name="language">The language name (Python, Lua, C, Java, Kotlin, Go).</param>
        /// <exception cref="ArgumentException">Thrown when the language is unknown.</exception>
        public static IExpressionSemantics ForLanguage(string language) => language switch
        {
            "Python" => new PythonExpressionSemantics(),
            "Lua" => new LuaExpressionSemantics(),
            "C" => new CExpressionSemantics(),
            "Java" => new JavaExpressionSemantics(),
            "Kotlin" => new KotlinExpressionSemantics(),
            "Go" => new GoExpressionSemantics(),
            _ => throw new ArgumentException($"Unknown language: {language}.", nameof(language)),
        };
    }
}
