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
using System.Text;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using Newtonsoft.Json;
using Serilog;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Base class for all declaration nodes in the abstract syntax tree.
    /// </summary>
    public abstract class DeclarationNode : ASTNode
    {
        /// <summary>
        /// Gets the children excluding leading tag nodes.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<ASTNode> ChildrenWithoutTags => this.Children.SkipWhile(e => e is TagNode);

        /// <summary>
        /// Gets the leading tag nodes.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<TagNode> Tags => this.Children.TakeWhile(e => e is TagNode).Cast<TagNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclarationNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected DeclarationNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclarationNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected DeclarationNode(int line, params ASTNode[] children)
            : base(line, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclarationNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="children">The child nodes.</param>
        protected DeclarationNode(int line, IEnumerable<TagNode> tags, IEnumerable<ASTNode> children)
            : base(line, tags.Concat(children)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclarationNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="children">The child nodes.</param>
        protected DeclarationNode(int line, IEnumerable<TagNode> tags, params ASTNode[] children)
            : base(line, tags.Concat(children)) { }
    }

    /// <summary>
    /// Represents declaration specifiers (type and modifiers).
    /// </summary>
    public sealed class DeclSpecsNode : DeclarationNode
    {
        /// <summary>
        /// Gets the modifiers parsed from the specifier string.
        /// </summary>
        public Modifiers Modifiers { get; }

        /// <summary>
        /// Gets the type name node.
        /// </summary>
        [JsonIgnore]
        public TypeNameNode TypeNode => this.Children.Single().As<TypeNameNode>();

        /// <summary>
        /// Gets the type name string.
        /// </summary>
        [JsonIgnore]
        public string TypeName => this.TypeNode.TypeName;

        /// <summary>
        /// Gets the resolved type, if any.
        /// </summary>
        [JsonIgnore]
        public Type? Type => this.TypeNode.Type;

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclSpecsNode"/> class with default type "object".
        /// </summary>
        /// <param name="line">The source line number.</param>
        public DeclSpecsNode(int line)
            : this(line, "object") { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclSpecsNode"/> class with a type name.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="type">The type name.</param>
        public DeclSpecsNode(int line, string type)
            : this(line, "", type) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclSpecsNode"/> class with modifiers and type.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="specs">The modifier string.</param>
        /// <param name="type">The type name.</param>
        public DeclSpecsNode(int line, string specs, string type)
            : base(line, new TypeNameNode(line, type))
        {
            this.Modifiers = Modifiers.Parse(specs);
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclSpecsNode"/> class with a type name node.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="type">The type name node.</param>
        public DeclSpecsNode(int line, TypeNameNode type)
            : this(line, "", type) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclSpecsNode"/> class with modifiers and a type name node.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="specs">The modifier string.</param>
        /// <param name="type">The type name node.</param>
        public DeclSpecsNode(int line, string specs, TypeNameNode type)
            : base(line, type)
        {
            this.Modifiers = Modifiers.Parse(specs);
        }

        /// <summary>
        /// Returns the text representation of the declaration specifiers.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder();
            string declSpecs = this.Modifiers.ToString();
            if (!string.IsNullOrWhiteSpace(declSpecs))
                sb.Append(declSpecs).Append(' ');
            sb.Append(this.TypeName);
            return sb.ToString();
        }

        /// <summary>
        /// Determines whether the specified object is equal to the current declaration specifiers.
        /// </summary>
        /// <param name="obj">The object to compare.</param>
        /// <returns>true if the objects are equal; otherwise, false.</returns>
        public override bool Equals(object? obj)
            => this.Equals(obj as DeclSpecsNode);

        /// <summary>
        /// Determines whether the specified node is equal to the current declaration specifiers.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns>true if the nodes are equal; otherwise, false.</returns>
        public override bool Equals([AllowNull] ASTNode other)
        {
            if (!base.Equals(other))
                return false;

            var decl = other as DeclSpecsNode;
            if (!this.Modifiers.Equals(decl?.Modifiers))
                return false;
            return this.Type is not null ? this.Type.Equals(decl?.Type) : this.TypeName.Equals(decl?.TypeName);
        }

        /// <summary>
        /// Returns a hash code for the declaration specifiers.
        /// </summary>
        /// <returns>A hash code.</returns>
        public override int GetHashCode() => HashCode.Combine(base.GetHashCode(), this.Modifiers, this.TypeName);
    }

    /// <summary>
    /// Base class for declaration nodes (variable, function, etc.).
    /// </summary>
    public abstract class DeclNode : DeclarationNode
    {
        /// <summary>
        /// Gets or sets the pointer indirection level.
        /// </summary>
        public int PointerLevel { get; set; }

        /// <summary>
        /// Gets the identifier node.
        /// </summary>
        [JsonIgnore]
        public IdNode IdentifierNode => this.Children.First().As<IdNode>();

        /// <summary>
        /// Gets the identifier name.
        /// </summary>
        [JsonIgnore]
        public string Identifier => this.IdentifierNode.Identifier;

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier node.</param>
        /// <param name="children">Additional child nodes.</param>
        public DeclNode(int line, IdNode identifier, params ASTNode[] children)
            : base(line, new[] { identifier }.Concat(children)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier node.</param>
        /// <param name="children">Additional child nodes.</param>
        public DeclNode(int line, IdNode identifier, IEnumerable<ASTNode> children)
            : base(line, new[] { identifier }.Concat(children)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The identifier node.</param>
        /// <param name="children">Additional child nodes.</param>
        public DeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, params ASTNode[] children)
            : base(line, tags.Concat(new ASTNode[] { identifier }).Concat(children)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The identifier node.</param>
        /// <param name="children">Additional child nodes.</param>
        public DeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, IEnumerable<ASTNode> children)
            : base(line, tags.Concat(new ASTNode[] { identifier }).Concat(children)) { }

        /// <summary>
        /// Returns the text representation of the declaration, including pointer levels.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{new string('*', this.PointerLevel)}{this.IdentifierNode.GetText()}";
    }

    /// <summary>
    /// Represents a list of declaration nodes.
    /// </summary>
    public class DeclListNode : DeclarationNode
    {
        /// <summary>
        /// Gets the collection of declarators.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<DeclNode> Declarators => this.Children.Cast<DeclNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="decls">The declaration nodes.</param>
        public DeclListNode(int line, IEnumerable<DeclNode> decls)
            : base(line, decls) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="decls">The declaration nodes.</param>
        public DeclListNode(int line, params DeclNode[] decls)
            : base(line, decls) { }

        /// <summary>
        /// Returns the text representation of the declaration list.
        /// </summary>
        /// <returns>A comma-separated string of declaration texts.</returns>
        public override string GetText() => string.Join(", ", this.Children.Select(c => c.GetText()));
    }

    /// <summary>
    /// Represents a variable declaration node.
    /// </summary>
    public sealed class VarDeclNode : DeclNode
    {
        /// <summary>
        /// Gets the initializer expression, if present.
        /// </summary>
        [JsonIgnore]
        public ExprNode? Initializer => this.Children.ElementAtOrDefault(1)?.As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="VarDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The variable identifier.</param>
        public VarDeclNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="VarDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The variable identifier.</param>
        /// <param name="initializer">The initializer expression.</param>
        public VarDeclNode(int line, IdNode identifier, ExprNode initializer)
            : base(line, identifier, initializer) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="VarDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The variable identifier.</param>
        public VarDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier)
            : base(line, tags, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="VarDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The variable identifier.</param>
        /// <param name="initializer">The initializer expression.</param>
        public VarDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, ExprNode initializer)
            : base(line, tags, identifier, initializer) { }

        /// <summary>
        /// Returns the text representation of the variable declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => this.Initializer is not null ? $"{base.GetText()} = {this.Initializer.GetText()}" : base.GetText();
    }

    /// <summary>
    /// Represents a list of type name nodes.
    /// </summary>
    public sealed class TypeNameListNode : DeclListNode
    {
        /// <summary>
        /// Gets the collection of type name nodes.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<TypeNameNode> Types => this.Children.Cast<TypeNameNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="TypeNameListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="decls">The type name nodes.</param>
        public TypeNameListNode(int line, IEnumerable<TypeNameNode> decls)
            : base(line, decls) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="TypeNameListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="decls">The type name nodes.</param>
        public TypeNameListNode(int line, params TypeNameNode[] decls)
            : base(line, decls) { }
    }

    /// <summary>
    /// Represents a type name node.
    /// </summary>
    public sealed class TypeNameNode : DeclNode
    {
        /// <summary>
        /// Gets the type name string.
        /// </summary>
        public string TypeName => this.Identifier;

        /// <summary>
        /// Gets the template type arguments, if any.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<TypeNameNode> TemplateArguments => this.Children.Skip(1).Cast<TypeNameNode>();

        /// <summary>
        /// Gets the resolved type, if any.
        /// </summary>
        [JsonIgnore]
        public Type? Type { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="TypeNameNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="typeName">The type name.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        public TypeNameNode(int line, string typeName, params TypeNameNode[] templateArgs)
            : this(line, typeName, templateArgs.AsEnumerable()) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="TypeNameNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="typeName">The type name.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        public TypeNameNode(int line, string typeName, IEnumerable<TypeNameNode> templateArgs)
            : base(line, new IdNode(line, typeName.Trim()), templateArgs)
        {
            TypeCode? typeCode = Types.TypeCodeFor(this.TypeName);
            if (typeCode is null)
                Log.Warning("Unknown type: {Type}", this.TypeName);
            else
                this.Type = Types.ToType(typeCode.Value);
        }

        /// <summary>
        /// Returns the text representation of the type name, including template arguments if present.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            if (!this.TemplateArguments.Any())
                return this.TypeName;

            var sb = new StringBuilder(this.TypeName);
            sb.Append('<').AppendJoin(", ", this.TemplateArguments).Append('>');
            return sb.ToString();
        }
    }
}
