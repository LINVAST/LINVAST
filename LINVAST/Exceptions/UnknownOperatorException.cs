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

namespace LINVAST.Exceptions
{
    /// <summary>Represents an error when an unknown operator is encountered.</summary>
    public sealed class UnknownOperatorException : ArgumentException
    {
        /// <summary>Gets the symbol of the unknown operator.</summary>
        public string? Symbol { get; }


        /// <summary>Initializes a new instance of the <see cref="UnknownOperatorException"/> class.</summary>
        public UnknownOperatorException()
        {

        }

        /// <summary>Initializes a new instance of the <see cref="UnknownOperatorException"/> class with a symbol.</summary>
        /// <param name="symbol">The unknown operator symbol.</param>
        public UnknownOperatorException(string symbol)
            : base($"Unknown operator: {symbol}")
        {
            this.Symbol = symbol;
        }

        /// <summary>Initializes a new instance of the <see cref="UnknownOperatorException"/> class with a symbol and inner exception.</summary>
        /// <param name="symbol">The unknown operator symbol.</param>
        /// <param name="innerException">The inner exception.</param>
        public UnknownOperatorException(string symbol, Exception? innerException)
            : base($"Unknown operator {symbol}", innerException)
        {
            this.Symbol = symbol;
        }

        /// <summary>Initializes a new instance of the <see cref="UnknownOperatorException"/> class with a symbol and parameter name.</summary>
        /// <param name="symbol">The unknown operator symbol.</param>
        /// <param name="paramName">The name of the parameter causing the exception.</param>
        public UnknownOperatorException(string symbol, string? paramName)
            : base($"Unknown operator {symbol}", paramName)
        {
            this.Symbol = symbol;
        }
    }
}
