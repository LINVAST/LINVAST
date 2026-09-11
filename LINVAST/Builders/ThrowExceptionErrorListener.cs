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


﻿using System.IO;
using Antlr4.Runtime;
using LINVAST.Exceptions;

namespace LINVAST.Builders
{
    /// <summary>An error listener that throws a <see cref="SyntaxErrorException"/> on syntax errors.</summary>
    public sealed class ThrowExceptionErrorListener : BaseErrorListener, IAntlrErrorListener<int>
    {
        /// <inheritdoc/>
        /// <exception cref="SyntaxErrorException">Always thrown when a syntax error is encountered.</exception>
        public override void SyntaxError(TextWriter output, IRecognizer recognizer, IToken symbol, int ln, int col, string msg, RecognitionException e)
            => throw new SyntaxErrorException(msg, ln, col, e);

        /// <inheritdoc/>
        /// <exception cref="SyntaxErrorException">Always thrown when a syntax error is encountered.</exception>
        public void SyntaxError(TextWriter output, IRecognizer recognizer, int symbol, int ln, int col, string msg, RecognitionException e)
            => throw new SyntaxErrorException(msg, ln, col, e);
    }
}
