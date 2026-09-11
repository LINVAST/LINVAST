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
    /// <summary>Represents an error that occurs during semantic analysis.</summary>
    public class SemanticErrorException : Exception
    {
        private static string FormErrorMessage(string msg, int line)
            => $"L{line}: {msg}";


        /// <summary>Initializes a new instance of the <see cref="SemanticErrorException"/> class with a message.</summary>
        /// <param name="message">The error message.</param>
        public SemanticErrorException(string message)
            : base(message)
        {

        }

        /// <summary>Initializes a new instance of the <see cref="SemanticErrorException"/> class with a message and inner exception.</summary>
        /// <param name="message">The error message.</param>
        /// <param name="innerException">The inner exception.</param>
        public SemanticErrorException(string message, Exception? innerException)
            : base(message, innerException)
        {

        }

        /// <summary>Initializes a new instance of the <see cref="SemanticErrorException"/> class with a message and line number.</summary>
        /// <param name="message">The error message.</param>
        /// <param name="line">The source line number.</param>
        public SemanticErrorException(string message, int line)
            : base(FormErrorMessage(message, line))
        {

        }

        /// <summary>Initializes a new instance of the <see cref="SemanticErrorException"/> class with a message, line number, and inner exception.</summary>
        /// <param name="message">The error message.</param>
        /// <param name="line">The source line number.</param>
        /// <param name="innerException">The inner exception.</param>
        public SemanticErrorException(string message, int line, Exception? innerException)
            : base(FormErrorMessage(message, line), innerException)
        {

        }
    }
}
