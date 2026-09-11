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
    /// <summary>Represents an error when a file extension or language is not supported.</summary>
    public sealed class UnsupportedLanguageException : ArgumentException
    {
        /// <summary>Initializes a new instance of the <see cref="UnsupportedLanguageException"/> class.</summary>
        public UnsupportedLanguageException()
            : base()
        {

        }

        /// <summary>Initializes a new instance of the <see cref="UnsupportedLanguageException"/> class with a message.</summary>
        /// <param name="message">The error message.</param>
        public UnsupportedLanguageException(string? message)
            : base(message ?? "Unsupported file extension")
        {

        }

        /// <summary>Initializes a new instance of the <see cref="UnsupportedLanguageException"/> class with a message and inner exception.</summary>
        /// <param name="message">The error message.</param>
        /// <param name="innerException">The inner exception.</param>
        public UnsupportedLanguageException(string? message, Exception? innerException)
            : base(message ?? "Unsupported file extension", innerException)
        {

        }

        /// <summary>Initializes a new instance of the <see cref="UnsupportedLanguageException"/> class with a message and parameter name.</summary>
        /// <param name="message">The error message.</param>
        /// <param name="paramName">The name of the parameter causing the exception.</param>
        public UnsupportedLanguageException(string? message, string? paramName)
            : base(message ?? "Unsupported file extension", paramName)
        {

        }
    }
}
