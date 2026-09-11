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


using System.Collections.Generic;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents the root source file node in the abstract syntax tree.
    /// </summary>
    public sealed class SourceNode : ASTNode
    {
        /// <summary>
        /// Gets or sets the name of the source file.
        /// </summary>
        public string? Name { get; set; }


        /// <summary>
        /// Initializes a new instance of the <see cref="SourceNode"/> class with the specified child nodes.
        /// </summary>
        /// <param name="children">The child nodes.</param>
        public SourceNode(IEnumerable<ASTNode> children)
            : base(1, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="SourceNode"/> class with the specified child nodes.
        /// </summary>
        /// <param name="children">The child nodes.</param>
        public SourceNode(params ASTNode[] children)
            : base(1, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="SourceNode"/> class with the specified name and child nodes.
        /// </summary>
        /// <param name="name">The source file name.</param>
        /// <param name="children">The child nodes.</param>
        public SourceNode(string name, IEnumerable<ASTNode> children)
            : base(1, children)
        {
            this.Name = name;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="SourceNode"/> class with the specified name and child nodes.
        /// </summary>
        /// <param name="name">The source file name.</param>
        /// <param name="children">The child nodes.</param>
        public SourceNode(string name, params ASTNode[] children)
            : base(1, children)
        {
            this.Name = name;
        }
    }
}
