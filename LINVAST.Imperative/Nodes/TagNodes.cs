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
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents a tag node with a name and optional fields in the abstract syntax tree.
    /// </summary>
    public sealed class TagNode : ASTNode
    {
        /// <summary>
        /// Gets the tag fields.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<TagFieldNode> Fields => this.Children.Skip(1).Cast<TagFieldNode>();

        /// <summary>
        /// Gets the identifier node of the tag.
        /// </summary>
        [JsonIgnore]
        public IdNode IdentifierNode => this.Children.First().As<IdNode>();

        /// <summary>
        /// Gets the tag identifier string.
        /// </summary>
        [JsonIgnore]
        public string Identifier => this.IdentifierNode.Identifier;


        /// <summary>
        /// Initializes a new instance of the <see cref="TagNode"/> class with a name.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="name">The tag name.</param>
        public TagNode(int line, string name)
            : base(line, new IdNode(line, name)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="TagNode"/> class with an identifier node.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="name">The identifier node for the tag name.</param>
        public TagNode(int line, IdNode name)
            : base(line, name) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="TagNode"/> class with a name and fields.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="name">The tag name.</param>
        /// <param name="fields">The tag fields.</param>
        public TagNode(int line, string name, IEnumerable<TagFieldNode> fields)
            : base(line, new ASTNode[] { new IdNode(line, name) }.Concat(fields)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="TagNode"/> class with an identifier node and fields.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="name">The identifier node for the tag name.</param>
        /// <param name="fields">The tag fields.</param>
        public TagNode(int line, IdNode name, params TagFieldNode[] fields)
            : base(line, new ASTNode[] { name }.Concat(fields)) { }


        /// <summary>
        /// Returns the text representation of the tag.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder(this.Identifier);
            if (this.Fields.Any())
                sb.Append('(').AppendJoin(", ", this.Fields).Append(')');
            return sb.ToString();
        }
    }


    /// <summary>
    /// Represents a field within a tag node.
    /// </summary>
    public sealed class TagFieldNode : ASTNode
    {
        /// <summary>
        /// Gets the field name.
        /// </summary>
        public string Name { get; }
        /// <summary>
        /// Gets the field value.
        /// </summary>
        public LitExprNode Value { get; }


        /// <summary>
        /// Initializes a new instance of the <see cref="TagFieldNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="name">The field name.</param>
        /// <param name="value">The field value.</param>
        public TagFieldNode(int line, string name, LitExprNode value)
            : base(line)
        {
            this.Name = name;
            this.Value = value;
        }


        /// <summary>
        /// Returns the text representation of the tag field.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"{this.Name}={this.Value}";

        /// <summary>
        /// Determines whether the specified object is equal to this <see cref="TagFieldNode"/>.
        /// </summary>
        /// <param name="obj">The object to compare.</param>
        /// <returns><c>true</c> if the object is equal; otherwise, <c>false</c>.</returns>
        public override bool Equals(object? obj)
            => this.Equals(obj as TagFieldNode);

        /// <summary>
        /// Determines whether the specified node is equal to this <see cref="TagFieldNode"/>.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns><c>true</c> if the node is equal; otherwise, <c>false</c>.</returns>
        public override bool Equals([AllowNull] ASTNode other)
        {
            if (!base.Equals(other))
                return false;

            var field = other as TagFieldNode;
            return Equals(this.Name, field?.Name) && Equals(this.Value, field?.Value);
        }

        /// <summary>
        /// Returns the hash code for this <see cref="TagFieldNode"/>.
        /// </summary>
        /// <returns>A hash code based on the field name.</returns>
        public override int GetHashCode() => this.Name.GetHashCode();
    }

    /// <summary>
    /// Defines the restrictions that can be applied to a tag.
    /// </summary>
    [Flags]
    public enum TagRestrictions
    {
        /// <summary>
        /// No restrictions specified.
        /// </summary>
        NotSpecified = 0,
        /// <summary>
        /// The tag applies to a type.
        /// </summary>
        Type = 1,
        /// <summary>
        /// The tag applies to a field.
        /// </summary>
        Field = 2,
        /// <summary>
        /// The tag applies to a property.
        /// </summary>
        Property = 4,
        /// <summary>
        /// The tag applies to a method.
        /// </summary>
        Method = 8,
        /// <summary>
        /// The tag applies to a constructor.
        /// </summary>
        Constructor = 16,
        /// <summary>
        /// The tag applies to a local variable.
        /// </summary>
        LocalVariable = 32,
        /// <summary>
        /// The tag applies to a local function.
        /// </summary>
        LocalFunction = 64,
        /// <summary>
        /// The tag applies to a function parameter.
        /// </summary>
        FunctionParameter = 128,
    }

    // TODO tag definition
}
