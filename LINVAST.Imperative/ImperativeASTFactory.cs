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
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using LINVAST.Builders;
using LINVAST.Exceptions;
using LINVAST.Nodes;

namespace LINVAST.Imperative
{
    /// <summary>
    /// Factory that builds an abstract syntax tree from a source file for the Imperative language.
    /// </summary>
    public sealed class ImperativeASTFactory : IASTFactory
    {
        /// <summary>
        /// Builds an AST from the specified source file.
        /// </summary>
        /// <param name="path">The path to the source file.</param>
        /// <returns>The root AST node.</returns>
        /// <exception cref="UnsupportedLanguageException">Thrown when no builder is registered for the file extension.</exception>
        /// <exception cref="AmbiguousMatchException">Thrown when multiple builders match the file extension.</exception>
        /// <exception cref="NotImplementedException">Thrown when the matched builder does not implement the required interface.</exception>
        public ASTNode BuildFromFile(string path)
        {
            var fi = new FileInfo(path);
            string code = File.ReadAllText(path);

            IEnumerable<Type> builderTypes = Assembly
                .GetAssembly(typeof(ImperativeASTFactory))
                .GetExportedTypes()
                .Where(t => t.GetCustomAttributes<ASTBuilderAttribute>()
                             .Any(a => string.Equals(a.FileExtension, fi.Extension, StringComparison.InvariantCultureIgnoreCase))
                )
                ;
            if (!builderTypes.Any())
                throw new UnsupportedLanguageException();

            Type? builderType = builderTypes.SingleOrDefault();
            if (builderType is null)
                throw new AmbiguousMatchException("Unique binder not registered to handle that file type.");

            if (!(Activator.CreateInstance(builderType) is IAbstractASTBuilder builder))
                throw new NotImplementedException("The builder for required file extension is found but does not inherit IAbstractASTBuilder class.");

            return builder.BuildFromSource(code);
        }
    }
}
