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


using LINVAST.Nodes;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents a package declaration node in the abstract syntax tree.
    /// </summary>
    public sealed class PackageNode : ASTNode
    {
        /// <summary>
        /// Gets the package name.
        /// </summary>
        public string Identifier { get; }


        /// <summary>
        /// Initializes a new instance of the <see cref="PackageNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The package name.</param>
        public PackageNode(int line, string identifier)
            : base(line)
        {
            this.Identifier = identifier;
        }


        /// <summary>
        /// Returns the text representation of the package declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string ToString() => "package " + this.Identifier;
    }
}
