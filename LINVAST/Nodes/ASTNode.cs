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
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using LINVAST.Exceptions;
using Newtonsoft.Json;
using Newtonsoft.Json.Converters;

namespace LINVAST.Nodes
{
    /// <summary>Represents a node in the language-invariant abstract syntax tree.</summary>
    public abstract class ASTNode : IEquatable<ASTNode>
    {
        /// <summary>Checks if two AST nodes are equal.</summary>
        public static bool operator ==(ASTNode x, ASTNode y) => x.Equals(y);
        /// <summary>Checks if two AST nodes are not equal.</summary>
        public static bool operator !=(ASTNode x, ASTNode y) => !(x == y);


        [JsonProperty(Order = 0)]
        /// <summary>Gets the name of the node type.</summary>
        public string NodeType => this.GetType().Name;

        [JsonIgnore]
        /// <summary>Gets or sets the parent node.</summary>
        public ASTNode? Parent { get; set; }

        [JsonProperty(Order = 1)]
        /// <summary>Gets the line number where this node appears in the source.</summary>
        public int Line { get; }

        [JsonProperty(Order = 2)]
        /// <summary>Gets the child nodes of this AST node.</summary>
        public IReadOnlyList<ASTNode> Children { get; private set; }


        /// <summary>Initializes a new instance of the <see cref="ASTNode"/> class.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected ASTNode(int line, params ASTNode[] children)
        {
            this.Children = children;
            this.Line = line;
            foreach (ASTNode child in this.Children) {
                if (child.Line < this.Line)
                    throw new ArgumentException("Parent node has greater line number than the child.");
                child.Parent = this;
            }
        }

        /// <summary>Initializes a new instance of the <see cref="ASTNode"/> class.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected ASTNode(int line, IEnumerable<ASTNode>? children)
            : this(line, children?.ToArray() ?? new ASTNode[]{ })
        {

        }


        /// <summary>Casts this node to the specified derived type.</summary>
        /// <typeparam name="T">The target AST node type.</typeparam>
        /// <returns>The node cast to <typeparamref name="T"/>.</returns>
        /// <exception cref="NodeMismatchException">Thrown when the node cannot be cast to <typeparamref name="T"/>.</exception>
        public T As<T>() where T : ASTNode
            => this as T ?? throw new NodeMismatchException(typeof(T), this.GetType());

        /// <summary>Gets the children of the specified type.</summary>
        /// <typeparam name="T">The type of children to retrieve.</typeparam>
        /// <returns>A collection of child nodes of type <typeparamref name="T"/>.</returns>
        public IEnumerable<T> ChildrenOfType<T>() => this.Children.Where(c => c is T).Cast<T>();

        /// <summary>Creates a deep copy of this node.</summary>
        /// <returns>A deep copy of this AST node.</returns>
        public ASTNode Copy()
        {
            var copy = (ASTNode)this.MemberwiseClone();
            copy.Children = this.Children
                .Select(c => c.Copy())
                .ToList()
                .AsReadOnly()
                ;
            return copy;
        }

        /// <summary>Creates a copy of this node with the specified node replaced.</summary>
        /// <param name="node">The node to replace.</param>
        /// <param name="replacement">The replacement node.</param>
        /// <returns>A new AST node with the substitution applied.</returns>
        public ASTNode Substitute(ASTNode? node, ASTNode? replacement)
            => node is null || replacement is null ? this.Copy() : this.Copy().SubstituteNode(node, replacement);

        /// <summary>Creates a copy of this node with the specified node replaced, cast to the target type.</summary>
        /// <typeparam name="T">The target AST node type.</typeparam>
        /// <param name="node">The node to replace.</param>
        /// <param name="replacement">The replacement node.</param>
        /// <returns>A new AST node of type <typeparamref name="T"/> with the substitution applied.</returns>
        /// <exception cref="NodeMismatchException">Thrown when the result cannot be cast to <typeparamref name="T"/>.</exception>
        public T Substitute<T>(ASTNode? node, ASTNode? replacement) where T : ASTNode
            => this.Substitute(node, replacement).As<T>();

        /// <summary>Serializes this node to a JSON string.</summary>
        /// <param name="compact">If <c>true</c>, produces compact JSON; otherwise, produces indented JSON.</param>
        /// <returns>A JSON string representation of this node.</returns>
        public string ToJson(bool compact = false)
            => JsonConvert.SerializeObject(this, compact ? Formatting.None : Formatting.Indented, new StringEnumConverter());

        /// <summary>Returns the text representation of this node.</summary>
        /// <returns>A string containing the node's text.</returns>
        public override string ToString()
            => this.GetText();

        /// <summary>Returns the hash code for this node.</summary>
        /// <returns>A hash code based on the node's text.</returns>
        public override int GetHashCode()
            => this.GetText().GetHashCode();

        /// <summary>Determines whether the specified object is equal to this node.</summary>
        /// <param name="obj">The object to compare.</param>
        /// <returns><c>true</c> if the object is equal; otherwise, <c>false</c>.</returns>
        public override bool Equals(object? obj)
            => this.Equals(obj as ASTNode);


        /// <summary>Determines whether the specified node is equal to this node.</summary>
        /// <param name="other">The node to compare.</param>
        /// <returns><c>true</c> if the node is equal; otherwise, <c>false</c>.</returns>
        public virtual bool Equals([AllowNull] ASTNode other)
        {
            if (other is null || this.GetType() != other.GetType())
                return false;

            if (ReferenceEquals(this, other))
                return true;

            if (this.Children.Count != other.Children.Count)
                return false;

            return this.Children.Zip(other.Children, (n1, n2) => (n1, n2)).All(tup => tup.n1.Equals(tup.n2));
        }

        /// <summary>Gets the concatenated text of all children.</summary>
        /// <returns>A string containing the text representation of this node and its children.</returns>
        public virtual string GetText()
            => string.Join(' ', this.Children.Select(c => c.GetText()));


        private ASTNode SubstituteNode(ASTNode node, ASTNode replacement)
        {
            if (node.Equals(this))
                return replacement;
            this.Children = this.Children
                .Select(c => c.Substitute(node, replacement))
                .ToList()
                .AsReadOnly()
                ;
            return this;
        }
    }
}
