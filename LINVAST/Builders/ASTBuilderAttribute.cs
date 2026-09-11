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
using System.Collections.Generic;

namespace LINVAST.Builders
{
    [AttributeUsage(AttributeTargets.Class, AllowMultiple = false)]
    /// <summary>Marks a class as an AST builder for a specific file extension.</summary>
    public sealed class ASTBuilderAttribute : Attribute
    {
        private static readonly HashSet<string> _fileExtensions = new();


        /// <summary>Gets the file extension associated with this builder.</summary>
        public string FileExtension { get; }


        /// <summary>Initializes a new instance of the <see cref="ASTBuilderAttribute"/> class.</summary>
        /// <param name="fileExtension">The file extension to associate with this builder.</param>
        public ASTBuilderAttribute(string fileExtension)
        {
            this.FileExtension = fileExtension;
            _fileExtensions.Add(fileExtension);
        }
    }
}
