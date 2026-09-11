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
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Represents a function declaration node.
    /// </summary>
    public sealed class FuncDeclNode : DeclNode
    {
        /// <summary>
        /// Gets a value indicating whether the function is variadic.
        /// </summary>
        [JsonIgnore]
        public bool IsVariadic => this.ParametersNode?.IsVariadic ?? false;

        /// <summary>
        /// Gets the template type arguments.
        /// </summary>
        [JsonIgnore]
        public TypeNameListNode TemplateArgs => this.Children[1].As<TypeNameListNode>();

        /// <summary>
        /// Gets the function parameters node, if present.
        /// </summary>
        [JsonIgnore]
        public FuncParamsNode? ParametersNode => this.Children.ElementAtOrDefault(2) as FuncParamsNode ?? null;

        /// <summary>
        /// Gets the function body block, if present.
        /// </summary>
        [JsonIgnore]
        public BlockStatNode? Definition => this.Children.Last() as BlockStatNode ?? null;

        /// <summary>
        /// Gets the function parameters.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<FuncParamNode>? Parameters => this.ParametersNode?.Parameters;

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        public FuncDeclNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="params">The function parameters.</param>
        public FuncDeclNode(int line, IdNode identifier, FuncParamsNode @params)
            : base(line, identifier, new TypeNameListNode(line), @params) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IdNode identifier, BlockStatNode body)
            : base(line, identifier, new TypeNameListNode(line), body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="params">The function parameters.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IdNode identifier, FuncParamsNode @params, BlockStatNode body)
            : base(line, identifier, new TypeNameListNode(line), @params, body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="params">The function parameters.</param>
        public FuncDeclNode(int line, IdNode identifier, TypeNameListNode templateArgs, FuncParamsNode @params)
            : base(line, identifier, templateArgs, @params) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IdNode identifier, TypeNameListNode templateArgs, BlockStatNode body)
            : base(line, identifier, templateArgs, body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="params">The function parameters.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IdNode identifier, TypeNameListNode templateArgs, FuncParamsNode @params, BlockStatNode body)
            : base(line, identifier, templateArgs, @params, body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier)
            : base(line, tags, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="params">The function parameters.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, FuncParamsNode @params)
            : base(line, tags, identifier, new TypeNameListNode(line), @params) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, BlockStatNode body)
            : base(line, tags, identifier, new TypeNameListNode(line), body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="params">The function parameters.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, FuncParamsNode @params, BlockStatNode body)
            : base(line, tags, identifier, new TypeNameListNode(line), @params, body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="params">The function parameters.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, TypeNameListNode templateArgs, FuncParamsNode @params)
            : base(line, tags, identifier, templateArgs, @params) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, TypeNameListNode templateArgs, BlockStatNode body)
            : base(line, tags, identifier, templateArgs, body) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncDeclNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="params">The function parameters.</param>
        /// <param name="body">The function body block.</param>
        public FuncDeclNode(int line, IEnumerable<TagNode> tags, IdNode identifier, TypeNameListNode templateArgs, FuncParamsNode @params, BlockStatNode body)
            : base(line, tags, identifier, templateArgs, @params, body) { }

        /// <summary>
        /// Returns the text representation of the function declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder();
            sb.Append(base.GetText()).Append('(');
            if (this.TemplateArgs is not null)
                sb.Append('<').AppendJoin(',', this.TemplateArgs.Types).Append('>');
            if (this.ParametersNode is not null)
                sb.Append(this.ParametersNode.GetText());
            sb.Append(')');
            if (this.Definition is not null)
                sb.Append(this.Definition.GetText());
            else
                sb.Append(';');
            return sb.ToString();
        }
    }

    /// <summary>
    /// Represents a lambda function expression node.
    /// </summary>
    public sealed class LambdaFuncExprNode : ExprNode
    {
        /// <summary>
        /// Gets the lambda body block.
        /// </summary>
        [JsonIgnore]
        public BlockStatNode Definition => this.Children.Last().As<BlockStatNode>();

        /// <summary>
        /// Gets the lambda parameters node, if present.
        /// </summary>
        [JsonIgnore]
        public FuncParamsNode? ParametersNode => this.Children.ElementAtOrDefault(0) as FuncParamsNode ?? null;

        /// <summary>
        /// Gets the lambda parameters.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<FuncParamNode>? Parameters => this.ParametersNode?.Parameters;

        /// <summary>
        /// Initializes a new instance of the <see cref="LambdaFuncExprNode"/> class without parameters.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="def">The lambda body block.</param>
        public LambdaFuncExprNode(int line, BlockStatNode def)
            : base(line, def)
        {

        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LambdaFuncExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="params">The lambda parameters.</param>
        /// <param name="def">The lambda body block.</param>
        public LambdaFuncExprNode(int line, FuncParamsNode @params, BlockStatNode def)
            : base(line, @params, def)
        {

        }

        /// <summary>
        /// Returns the text representation of the lambda expression.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"lambda ({this.ParametersNode?.GetText() ?? ""}): {this.Definition.GetText()}";
    }

    /// <summary>
    /// Represents a function declaration statement node.
    /// </summary>
    public sealed class FuncNode : DeclStatNode
    {
        /// <summary>
        /// Gets the function declarator.
        /// </summary>
        [JsonIgnore]
        public FuncDeclNode Declarator => this.ChildrenWithoutTags.ElementAt(1).As<DeclListNode>().Declarators.Single().As<FuncDeclNode>();

        /// <summary>
        /// Gets the function body block, if present.
        /// </summary>
        [JsonIgnore]
        public BlockStatNode? Definition => this.Declarator.Definition;

        /// <summary>
        /// Gets the return type name.
        /// </summary>
        [JsonIgnore]
        public string ReturnTypeName => this.Specifiers.TypeName;

        /// <summary>
        /// Gets the resolved return type, if any.
        /// </summary>
        [JsonIgnore]
        public Type? ReturnType => this.Specifiers.Type;

        /// <summary>
        /// Gets the function identifier.
        /// </summary>
        [JsonIgnore]
        public string Identifier => this.Declarator.Identifier;

        /// <summary>
        /// Gets a value indicating whether the function is variadic.
        /// </summary>
        [JsonIgnore]
        public bool IsVariadic => this.Declarator.IsVariadic;

        /// <summary>
        /// Gets the function parameters node.
        /// </summary>
        [JsonIgnore]
        public FuncParamsNode? ParametersNode => this.Declarator.ParametersNode;

        /// <summary>
        /// Gets the function parameters.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<FuncParamNode>? Parameters => this.ParametersNode?.Parameters;

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="declSpecs">The declaration specifiers (return type and modifiers).</param>
        /// <param name="decl">The function declarator.</param>
        public FuncNode(int line, DeclSpecsNode declSpecs, FuncDeclNode decl)
            : base(line, declSpecs, new DeclListNode(line, decl)) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="declSpecs">The declaration specifiers.</param>
        /// <param name="decl">The function declarator.</param>
        public FuncNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode declSpecs, FuncDeclNode decl)
            : base(line, tags, declSpecs, new DeclListNode(line, decl)) { }

        /// <summary>
        /// Returns the text representation of the function declaration.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"{string.Join(' ', this.Tags)} {this.Modifiers} {this.Declarator.GetText()}";
    }

    /// <summary>
    /// Represents a function parameter list node.
    /// </summary>
    public sealed class FuncParamsNode : DeclarationNode
    {
        /// <summary>
        /// Gets or sets a value indicating whether the parameter list is variadic.
        /// </summary>
        public bool IsVariadic { get; set; }

        /// <summary>
        /// Gets the collection of function parameters.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<FuncParamNode> Parameters => this.Children.Cast<FuncParamNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncParamsNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="params">The function parameters.</param>
        public FuncParamsNode(int line, IEnumerable<FuncParamNode> @params)
            : base(line, @params) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncParamsNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="params">The function parameters.</param>
        public FuncParamsNode(int line, params FuncParamNode[] @params)
            : base(line, @params) { }

        /// <summary>
        /// Returns the text representation of the parameter list.
        /// </summary>
        /// <returns>A comma-separated string of parameter texts.</returns>
        public override string GetText() => string.Join(", ", this.Children.Select(c => c.GetText()));

        /// <summary>
        /// Determines whether the specified node is equal to the current parameter list.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns>true if the nodes are equal; otherwise, false.</returns>
        public override bool Equals([AllowNull] ASTNode other)
            => base.Equals(other) && this.IsVariadic.Equals((other as FuncParamsNode)?.IsVariadic);
    }

    /// <summary>
    /// Represents a single function parameter node.
    /// </summary>
    public class FuncParamNode : DeclarationNode
    {
        /// <summary>
        /// Gets the parameter declaration specifiers (type and modifiers).
        /// </summary>
        [JsonIgnore]
        public DeclSpecsNode Specifiers => this.Children[0].As<DeclSpecsNode>();

        /// <summary>
        /// Gets the parameter declarator.
        /// </summary>
        [JsonIgnore]
        public DeclNode Declarator => this.Children[1].As<DeclNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncParamNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="declSpecs">The declaration specifiers.</param>
        /// <param name="declarator">The declarator node.</param>
        public FuncParamNode(int line, DeclSpecsNode declSpecs, DeclNode declarator)
            : base(line, declSpecs, declarator) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncParamNode"/> class with tags.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="declSpecs">The declaration specifiers.</param>
        /// <param name="declarator">The declarator node.</param>
        public FuncParamNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode declSpecs, DeclNode declarator)
            : base(line, tags, declSpecs, declarator) { }
    }
}
