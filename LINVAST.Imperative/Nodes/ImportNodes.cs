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
using System.Linq;
using System.Text;
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents a list of import declarations in the abstract syntax tree.
    /// </summary>
    public sealed class ImportListNode : ASTNode
    {
        /// <summary>
        /// Gets the import declarations.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<ImportNode> Imports => this.Children.Cast<ImportNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="ImportListNode"/> class with an enumerable of imports.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="imports">The import declarations.</param>
        public ImportListNode(int line, IEnumerable<ImportNode> imports)
            : base(line, imports) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ImportListNode"/> class with a params array of imports.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="imports">The import declarations.</param>
        public ImportListNode(int line, params ImportNode[] imports)
            : base(line, imports) { }


        /// <summary>
        /// Returns the text representation of the import list.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string ToString() => string.Join('\n', this.Imports);
    }

    /// <summary>
    /// Represents a single import declaration in the abstract syntax tree.
    /// </summary>
    public sealed class ImportNode : ASTNode
    {
        /// <summary>
        /// Gets the import directive path.
        /// </summary>
        public string Directive { get; }
        /// <summary>
        /// Gets the qualified name alias, if specified.
        /// </summary>
        public string? QualifiedAs { get; }


        /// <summary>
        /// Initializes a new instance of the <see cref="ImportNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="directive">The import directive path.</param>
        /// <param name="qualifiedAs">The qualified name alias, or <c>null</c> if no alias is specified.</param>
        public ImportNode(int line, string directive, string? qualifiedAs = null)
            : base(line)
        {
            this.Directive = directive;
            this.QualifiedAs = qualifiedAs;
        }


        /// <summary>
        /// Returns the text representation of the import declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string ToString()
        {
            var sb = new StringBuilder("import ");
            sb.Append(this.Directive);
            if (this.QualifiedAs is not null)
                sb.Append(" as ").Append(this.QualifiedAs);
            sb.AppendLine();
            return sb.ToString();
        }
    }
}
