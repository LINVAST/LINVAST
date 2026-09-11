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


﻿using LINVAST.Nodes;

namespace LINVAST
{
    /// <summary>Defines a contract for creating an AST from a file.</summary>
    public interface IASTFactory
    {
        /// <summary>Builds an AST from the specified file.</summary>
        /// <param name="path">The path to the source file.</param>
        /// <returns>The root AST node.</returns>
        ASTNode BuildFromFile(string path);
    }
}
