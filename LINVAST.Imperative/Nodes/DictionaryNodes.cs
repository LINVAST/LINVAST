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
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents a dictionary declaration node in the abstract syntax tree.
    /// </summary>
    public sealed class DictDeclNode : DeclNode
    {
        /// <summary>
        /// Gets the dictionary initializer, if specified.
        /// </summary>
        [JsonIgnore]
        public DictInitNode? Initializer => this.Children.ElementAtOrDefault(1)?.As<DictInitNode>();


        /// <summary>
        /// Initializes a new instance of the <see cref="DictDeclNode"/> class with an identifier.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier for the dictionary.</param>
        public DictDeclNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DictDeclNode"/> class with an identifier and initializer.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier for the dictionary.</param>
        /// <param name="initializer">The dictionary initializer.</param>
        public DictDeclNode(int line, IdNode identifier, DictInitNode initializer)
            : base(line, identifier, initializer) { }


        /// <summary>
        /// Returns the text representation of the dictionary declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{base.GetText()} = {this.Initializer?.GetText() ?? "{{}"}";
    }

    /// <summary>
    /// Represents a key-value entry in a dictionary initializer.
    /// </summary>
    public sealed class DictEntryNode : ExprNode
    {
        /// <summary>
        /// Gets the key of the dictionary entry.
        /// </summary>
        [JsonIgnore]
        public IdNode Key => this.Children[0].As<IdNode>();

        /// <summary>
        /// Gets the value of the dictionary entry.
        /// </summary>
        [JsonIgnore]
        public ExprNode Value => this.Children[1].As<ExprNode>();


        /// <summary>
        /// Initializes a new instance of the <see cref="DictEntryNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="key">The key identifier.</param>
        /// <param name="value">The value expression.</param>
        public DictEntryNode(int line, IdNode key, ExprNode value)
            : base(line, key, value) { }


        /// <summary>
        /// Returns the text representation of the dictionary entry.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"'{this.Key}' : {this.Value}";
    }

    /// <summary>
    /// Represents a dictionary initializer expression node in the abstract syntax tree.
    /// </summary>
    public sealed class DictInitNode : ExprListNode
    {
        /// <summary>
        /// Gets the entries in the dictionary initializer.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<DictEntryNode> Entries => this.Children.Cast<DictEntryNode>();


        /// <summary>
        /// Initializes a new instance of the <see cref="DictInitNode"/> class with an enumerable of entries.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="entries">The dictionary entries.</param>
        public DictInitNode(int line, IEnumerable<DictEntryNode> entries)
            : base(line, entries) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DictInitNode"/> class with a params array of entries.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="entries">The dictionary entries.</param>
        public DictInitNode(int line, params DictEntryNode[] entries)
            : base(line, entries) { }


        /// <summary>
        /// Returns the text representation of the dictionary initializer.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => new StringBuilder("{ ").AppendJoin(", ", this.Entries.Select(e => e.GetText())).Append(" }").ToString();
    }
}
