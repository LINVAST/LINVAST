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


﻿using System.Collections.Generic;
using System.Linq;
using System.Text;
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents a type declaration node in the abstract syntax tree.
    /// </summary>
    public sealed class TypeDeclNode : DeclNode
    {
        /// <summary>
        /// Gets the declarations within the type.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<DeclStatNode> Declarations => this.Children.Skip(3).Cast<DeclStatNode>();

        /// <summary>
        /// Gets the template parameters of the type.
        /// </summary>
        [JsonIgnore]
        public TypeNameListNode TemplateParameters => this.Children[1].As<TypeNameListNode>();

        /// <summary>
        /// Gets the base types of the type.
        /// </summary>
        [JsonIgnore]
        public TypeNameListNode BaseTypes => this.Children[2].As<TypeNameListNode>();


        /// <summary>
        /// Initializes a new instance of the <see cref="TypeDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The type identifier node.</param>
        /// <param name="templateParams">The template parameters.</param>
        /// <param name="baseTypes">The base types.</param>
        /// <param name="declarations">The declarations within the type.</param>
        public TypeDeclNode(int line, IdNode identifier, TypeNameListNode templateParams, TypeNameListNode baseTypes, IEnumerable<DeclStatNode> declarations)
            : base(line, identifier, new ASTNode[] { templateParams, baseTypes }.Concat(declarations)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="TypeDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The type identifier node.</param>
        /// <param name="templateParams">The template parameters.</param>
        /// <param name="baseTypes">The base types.</param>
        /// <param name="declarations">The declarations within the type.</param>
        public TypeDeclNode(int line, IdNode identifier, TypeNameListNode templateParams, TypeNameListNode baseTypes, params DeclStatNode[] declarations)
            : base(line, identifier, new ASTNode[] { templateParams, baseTypes }.Concat(declarations)) { }


        /// <summary>
        /// Returns the text representation of the type declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder();
            sb.Append(this.Identifier);
            if (this.TemplateParameters.Types.Any())
                sb.Append('<').Append(this.TemplateParameters.Types).Append('>');
            if (this.BaseTypes.Types.Any())
                sb.Append(" : ").Append(this.BaseTypes.Types);
            sb.AppendLine();
            sb.AppendLine(" { ").AppendJoin("; ", this.Declarations).AppendLine(" }");
            return sb.ToString();
        }
    }

    /// <summary>
    /// Represents an enumeration declaration node in the abstract syntax tree.
    /// </summary>
    public sealed class EnumDeclNode : DeclNode
    {
        /// <summary>
        /// Gets the enumeration constants.
        /// </summary>
        [JsonIgnore]
        public DeclListNode Constants => this.Children.ElementAt(1).As<DeclListNode>();

        /// <summary>
        /// Gets the body declarations of the enumeration.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<DeclStatNode> BodyDeclarations => this.Children.Skip(1).Select(c => c.As<DeclStatNode>());


        /// <summary>
        /// Initializes a new instance of the <see cref="EnumDeclNode"/> class with an identifier.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The enumeration identifier node.</param>
        public EnumDeclNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="EnumDeclNode"/> class with an identifier and constants.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The enumeration identifier node.</param>
        /// <param name="constants">The enumeration constants.</param>
        public EnumDeclNode(int line, IdNode identifier, DeclListNode constants)
            : base(line, identifier, constants) { }
        
        /// <summary>
        /// Initializes a new instance of the <see cref="EnumDeclNode"/> class with an identifier, constants, and body.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The enumeration identifier node.</param>
        /// <param name="constants">The enumeration constants.</param>
        /// <param name="body">The body declarations.</param>
        public EnumDeclNode(int line, IdNode identifier, DeclListNode constants, IEnumerable<ASTNode> body)
            : base(line, identifier, new ASTNode[] { constants }.Concat(body)) { }
    }

    /// <summary>
    /// Abstract base class for all type nodes in the abstract syntax tree.
    /// </summary>
    public abstract class TypeNode : DeclStatNode
    {
        /// <summary>
        /// Gets the category of the type.
        /// </summary>
        protected string Category { get; }


        /// <summary>
        /// Initializes a new instance of the <see cref="TypeNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="category">The type category (e.g., "class", "struct").</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        protected TypeNode(int line, string category, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, specifiers, new DeclListNode(line, decl))
        {
            this.Category = category;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="TypeNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="category">The type category (e.g., "class", "struct").</param>
        /// <param name="tags">The tags applied to the type.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        protected TypeNode(int line, string category, IEnumerable<TagNode> tags, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, tags, specifiers, new DeclListNode(line, decl))
        {
            this.Category = category;
        }


        /// <summary>
        /// Returns the text representation of the type node.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{string.Join(' ', this.Tags)} {this.Specifiers} {this.Category} {this.DeclaratorList}";
    }

    /// <summary>
    /// Represents a class type node in the abstract syntax tree.
    /// </summary>
    public sealed class ClassNode : TypeNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ClassNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        public ClassNode(int line, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, "class", specifiers, decl) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ClassNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The tags applied to the class.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        public ClassNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, "class", tags, specifiers, decl) { }
    }

    /// <summary>
    /// Represents a struct type node in the abstract syntax tree.
    /// </summary>
    public sealed class StructNode : TypeNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="StructNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        public StructNode(int line, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, "struct", specifiers, decl) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="StructNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The tags applied to the struct.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        public StructNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, "struct", tags, specifiers, decl) { }
    }

    /// <summary>
    /// Represents an interface type node in the abstract syntax tree.
    /// </summary>
    public sealed class InterfaceNode : TypeNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="InterfaceNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        public InterfaceNode(int line, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, "interface", specifiers, decl) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="InterfaceNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The tags applied to the interface.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The type declaration node.</param>
        public InterfaceNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode specifiers, TypeDeclNode decl)
            : base(line, "interface", tags, specifiers, decl) { }
    }

    /// <summary>
    /// Represents an enumeration node in the abstract syntax tree.
    /// </summary>
    public sealed class EnumNode : DeclStatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="EnumNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The enumeration declaration node.</param>
        public EnumNode(int line, DeclSpecsNode specifiers, EnumDeclNode decl)
            : base(line, specifiers, new DeclListNode(line, decl)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="EnumNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The tags applied to the enumeration.</param>
        /// <param name="specifiers">The declaration specifiers.</param>
        /// <param name="decl">The enumeration declaration node.</param>
        public EnumNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode specifiers, EnumDeclNode decl)
            : base(line, tags, specifiers, new DeclListNode(line, decl)) { }

        /// <summary>
        /// Returns the text representation of the enumeration node.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string ToString() => $"{string.Join(' ', this.Tags)} {this.Specifiers} enum {this.DeclaratorList}";
    }
}
