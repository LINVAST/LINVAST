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
    /// <summary>Represents an error when an AST node is of an unexpected type.</summary>
    public sealed class NodeMismatchException : SyntaxErrorException
    {
        /// <summary>Initializes a new instance of the <see cref="NodeMismatchException"/> class.</summary>
        /// <param name="expected">The expected node type.</param>
        /// <param name="actual">The actual node type.</param>
        public NodeMismatchException(Type expected, Type actual)
            : base($"expected: {expected.Name}, got: {actual.Name}")
        {

        }
    }
}
