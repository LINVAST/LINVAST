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
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Base class for all expression nodes in the abstract syntax tree.
    /// </summary>
    public abstract class ExprNode : ASTNode
    {
        protected ExprNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }

        protected ExprNode(int line, params ASTNode[] children)
            : base(line, children) { }
    }

    /// <summary>
    /// Represents a list of expression nodes.
    /// </summary>
    public class ExprListNode : ExprNode
    {
        /// <summary>
        /// Gets the collection of expressions.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<ExprNode> Expressions => this.Children.Cast<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="ExprListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expressions">The expressions to include.</param>
        public ExprListNode(int line, params ExprNode[] expressions)
            : base(line, expressions) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ExprListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expressions">The expressions to include.</param>
        public ExprListNode(int line, IEnumerable<ExprNode> expressions)
            : base(line, expressions) { }

        /// <summary>
        /// Returns the text representation of the expression list.
        /// </summary>
        /// <returns>A comma-separated string of expression texts.</returns>
        public override string GetText() => string.Join(", ", this.Children.Select(c => c.GetText()));
    }

    /// <summary>
    /// Represents a list of identifier nodes.
    /// </summary>
    public sealed class IdListNode : ExprListNode
    {
        /// <summary>
        /// Gets the collection of identifiers.
        /// </summary>
        [JsonIgnore]
        public IEnumerable<IdNode> Identifiers => this.Children.Cast<IdNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="IdListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expressions">The identifier nodes to include.</param>
        public IdListNode(int line, params IdNode[] expressions)
            : base(line, expressions) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="IdListNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expressions">The identifier nodes to include.</param>
        public IdListNode(int line, IEnumerable<IdNode> expressions)
            : base(line, expressions) { }
    }

    /// <summary>
    /// Represents a unary expression node.
    /// </summary>
    public sealed class UnaryExprNode : ExprNode
    {
        /// <summary>
        /// Gets the unary operator.
        /// </summary>
        [JsonIgnore]
        public UnaryOpNode Operator => this.Children[0].As<UnaryOpNode>();

        /// <summary>
        /// Gets the operand of the unary expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Operand => this.Children[1].As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="UnaryExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="operator">The unary operator.</param>
        /// <param name="operand">The operand expression.</param>
        public UnaryExprNode(int line, UnaryOpNode @operator, ExprNode operand)
            : base(line, @operator, operand) { }

        /// <summary>
        /// Returns the text representation of the unary expression.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{this.Operator}({this.Operand})";
    }

    /// <summary>
    /// Base class for all binary expression nodes.
    /// </summary>
    public abstract class BinaryExprNode : ExprNode
    {
        /// <summary>
        /// Gets the binary operator.
        /// </summary>
        [JsonIgnore]
        public BinaryOpNode Operator => this.Children[1].As<BinaryOpNode>();

        /// <summary>
        /// Gets the left operand.
        /// </summary>
        [JsonIgnore]
        public ExprNode LeftOperand => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Gets the right operand.
        /// </summary>
        [JsonIgnore]
        public ExprNode RightOperand => this.Children[2].As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="BinaryExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="left">The left operand.</param>
        /// <param name="operator">The binary operator.</param>
        /// <param name="right">The right operand.</param>
        protected BinaryExprNode(int line, ExprNode left, BinaryOpNode @operator, ExprNode right)
            : base(line, left, @operator, right) { }

        /// <summary>
        /// Returns the text representation of the binary expression.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"({this.LeftOperand} {this.Operator} {this.RightOperand})";
    }

    /// <summary>
    /// Represents an arithmetic expression node.
    /// </summary>
    public sealed class ArithmExprNode : BinaryExprNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ArithmExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="left">The left operand.</param>
        /// <param name="operator">The arithmetic operator.</param>
        /// <param name="right">The right operand.</param>
        public ArithmExprNode(int line, ExprNode left, ArithmOpNode @operator, ExprNode right)
            : base(line, left, @operator, right) { }
    }

    /// <summary>
    /// Represents a logical expression node.
    /// </summary>
    public sealed class LogicExprNode : BinaryExprNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="LogicExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="left">The left operand.</param>
        /// <param name="operator">The logical operator.</param>
        /// <param name="right">The right operand.</param>
        public LogicExprNode(int line, ExprNode left, BinaryLogicOpNode @operator, ExprNode right)
            : base(line, left, @operator, right) { }
    }

    /// <summary>
    /// Represents a relational expression node.
    /// </summary>
    public sealed class RelExprNode : BinaryExprNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="RelExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="left">The left operand.</param>
        /// <param name="operator">The relational operator.</param>
        /// <param name="right">The right operand.</param>
        public RelExprNode(int line, ExprNode left, RelOpNode @operator, ExprNode right)
            : base(line, left, @operator, right) { }
    }

    /// <summary>
    /// Represents an assignment expression node.
    /// </summary>
    public class AssignExprNode : BinaryExprNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="AssignExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="left">The left-hand side expression.</param>
        /// <param name="operator">The assignment operator.</param>
        /// <param name="right">The right-hand side expression.</param>
        public AssignExprNode(int line, ExprNode left, AssignOpNode @operator, ExprNode right)
            : base(line, left, @operator, right) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="AssignExprNode"/> class with a simple assignment operator.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="left">The left-hand side expression.</param>
        /// <param name="right">The right-hand side expression.</param>
        public AssignExprNode(int line, ExprNode left, ExprNode right)
            : base(line, left, AssignOpNode.FromSymbol(line, "="), right) { }

        /// <summary>
        /// Simplifies a complex assignment by expanding compound operators (e.g., +=, -=) into their equivalent binary operation plus simple assignment.
        /// </summary>
        /// <returns>The simplified assignment expression.</returns>
        public AssignExprNode SimplifyComplexAssignment()
        {
            if (this.Operator is ComplexAssignOpNode && this.Operator.Symbol.Length > 1) {
                string part = this.Operator.Symbol.Substring(0, this.Operator.Symbol.IndexOf('='));
                var expanded = new ArithmExprNode(this.Line,
                    this.LeftOperand,
                    ArithmOpNode.FromSymbol(this.Operator.Line, part),
                    this.RightOperand
                );
                return new AssignExprNode(this.Line, this.LeftOperand, expanded);
            }
            return this;
        }
    }

    /// <summary>
    /// Represents an identifier node.
    /// </summary>
    public sealed class IdNode : ExprNode
    {
        /// <summary>
        /// Gets the identifier name.
        /// </summary>
        public string Identifier { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="IdNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier name.</param>
        /// <exception cref="ArgumentException">Thrown when the identifier is null or whitespace.</exception>
        public IdNode(int line, string identifier)
            : base(line)
        {
            if (string.IsNullOrWhiteSpace(identifier))
                throw new ArgumentException("Identifier name must be set.");
            this.Identifier = identifier;
        }

        /// <summary>
        /// Returns the text representation of the identifier.
        /// </summary>
        /// <returns>The identifier name.</returns>
        public override string GetText() => this.Identifier;

        /// <summary>
        /// Determines whether the specified node is equal to the current identifier.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns>true if the nodes are equal; otherwise, false.</returns>
        public override bool Equals([AllowNull] ASTNode other)
            => base.Equals(other) && this.Identifier.Equals(other.As<IdNode>().Identifier);
    }

    /// <summary>
    /// Represents a function call expression node.
    /// </summary>
    public class FuncCallExprNode : ExprNode
    {
        /// <summary>
        /// Gets the function identifier.
        /// </summary>
        [JsonIgnore]
        public string Identifier => this.Children[0].As<IdNode>().Identifier;

        /// <summary>
        /// Gets the template type arguments, if any.
        /// </summary>
        [JsonIgnore]
        public TypeNameListNode? TemplateArguments => this.Children.ElementAtOrDefault(1) as TypeNameListNode;

        /// <summary>
        /// Gets the call arguments, if any.
        /// </summary>
        [JsonIgnore]
        public ExprListNode? Arguments
            => this.Children.Count > 2 ? this.Children[2].As<ExprListNode>()
                                       : this.Children.ElementAtOrDefault(1) as ExprListNode;

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncCallExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        public FuncCallExprNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncCallExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="args">The call arguments.</param>
        public FuncCallExprNode(int line, IdNode identifier, ExprListNode args)
            : base(line, identifier, args) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncCallExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        public FuncCallExprNode(int line, IdNode identifier, TypeNameListNode templateArgs)
            : base(line, identifier, templateArgs) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="FuncCallExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The function identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="args">The call arguments.</param>
        public FuncCallExprNode(int line, IdNode identifier, TypeNameListNode templateArgs, ExprListNode args)
            : base(line, identifier, templateArgs, args) { }

        /// <summary>
        /// Returns the text representation of the function call.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{this.Identifier}<{(this.TemplateArguments?.GetText() ?? "")}>({this.Arguments?.GetText() ?? ""})";
    }

    /// <summary>
    /// Represents a constructor expression node.
    /// </summary>
    public sealed class ConsExprNode : FuncCallExprNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ConsExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The type identifier.</param>
        public ConsExprNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ConsExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The type identifier.</param>
        /// <param name="args">The constructor arguments.</param>
        public ConsExprNode(int line, IdNode identifier, ExprListNode args)
            : base(line, identifier, args) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ConsExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The type identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        public ConsExprNode(int line, IdNode identifier, TypeNameListNode templateArgs)
            : base(line, identifier, templateArgs) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ConsExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The type identifier.</param>
        /// <param name="templateArgs">The template type arguments.</param>
        /// <param name="args">The constructor arguments.</param>
        public ConsExprNode(int line, IdNode identifier, TypeNameListNode templateArgs, ExprListNode args)
            : base(line, identifier, templateArgs, args) { }
    }

    /// <summary>
    /// Represents an array access expression node.
    /// </summary>
    public sealed class ArrAccessExprNode : ExprNode
    {
        /// <summary>
        /// Gets the array expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Array => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Gets the index expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode IndexExpression => this.Children[1].As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="ArrAccessExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="array">The array expression.</param>
        /// <param name="indexExpr">The index expression.</param>
        public ArrAccessExprNode(int line, ExprNode array, ExprNode indexExpr)
            : base(line, array, indexExpr) { }

        /// <summary>
        /// Returns the text representation of the array access.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{this.Array.GetText()}[{this.IndexExpression.GetText()}]";
    }

    /// <summary>
    /// Represents an increment expression node (e.g., x += 1).
    /// </summary>
    public sealed class IncExprNode : AssignExprNode
    {
        /// <summary>
        /// Gets the expression being incremented.
        /// </summary>
        [JsonIgnore]
        public ExprNode Expr => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="IncExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expr">The expression to increment.</param>
        public IncExprNode(int line, ExprNode expr)
            : base(line, expr, AssignOpNode.FromSymbol(line, "+="), new LitExprNode(line, 1)) { }

        /// <summary>
        /// Returns the text representation of the increment expression.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{this.LeftOperand.GetText()}++";
    }

    /// <summary>
    /// Represents a decrement expression node (e.g., x -= 1).
    /// </summary>
    public sealed class DecExprNode : AssignExprNode
    {
        /// <summary>
        /// Gets the expression being decremented.
        /// </summary>
        [JsonIgnore]
        public ExprNode Expr => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="DecExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expr">The expression to decrement.</param>
        public DecExprNode(int line, ExprNode expr)
            : base(line, expr, AssignOpNode.FromSymbol(line, "-="), new LitExprNode(line, 1)) { }

        /// <summary>
        /// Returns the text representation of the decrement expression.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{this.Expr.GetText()}--";
    }

    /// <summary>
    /// Represents a literal expression node.
    /// </summary>
    public class LitExprNode : ExprNode
    {
        /// <summary>
        /// Creates a literal expression node from a string representation.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="str">The string representation of the literal.</param>
        /// <returns>A literal expression node.</returns>
        /// <exception cref="NotImplementedException">Thrown when the literal format is not supported.</exception>
        public static LitExprNode FromString(int line, string str)
        {
            if (!Constants.TryConvert(str, out object? value, out string? suffix))
                throw new NotImplementedException($"Literal {str} is not supported");
            if (value is null)
                return new NullLitExprNode(line);
            return new LitExprNode(line, value, suffix);
        }

        /// <summary>
        /// Gets the literal value.
        /// </summary>
        public object? Value { get; }

        /// <summary>
        /// Gets the literal suffix, if any.
        /// </summary>
        public string? Suffix { get; }

        /// <summary>
        /// Gets the type code of the literal value.
        /// </summary>
        public TypeCode TypeCode { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="LitExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="value">The literal value.</param>
        /// <param name="suffix">The optional suffix.</param>
        /// <exception cref="ArgumentNullException">Thrown when the value is null.</exception>
        public LitExprNode(int line, object value, string? suffix = null)
            : base(line)
        {
            if (value is null)
                throw new ArgumentNullException("Value cannot be null. Use NullLiteralNode instead.");
            this.Suffix = suffix?.ToUpper();
            this.Value = value;
            this.TypeCode = Type.GetTypeCode(value.GetType());
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="LitExprNode"/> class with a specific type code.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="value">The literal value.</param>
        /// <param name="typeCode">The type code.</param>
        /// <param name="suffix">The optional suffix.</param>
        protected LitExprNode(int line, object? value, TypeCode typeCode, string? suffix = null)
            : base(line)
        {
            this.Suffix = suffix?.ToUpper();
            this.Value = value;
            this.TypeCode = typeCode;
        }

        /// <summary>
        /// Returns the text representation of the literal.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => this.Value is string str ? $"\"{str}\"" : this.Value?.ToString() ?? "";

        /// <summary>
        /// Determines whether the specified node is equal to the current literal.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns>true if the nodes are equal; otherwise, false.</returns>
        public override bool Equals([AllowNull] ASTNode other)
        {
            var lit = other as LitExprNode;
            if (!base.Equals(other) || !this.TypeCode.Equals(lit?.TypeCode))
                return false;
            if (this.Value is null)
                return lit.Value is null;
            return this.Value.Equals(lit.Value);
        }
    }

    /// <summary>
    /// Represents a null literal expression node.
    /// </summary>
    public sealed class NullLitExprNode : LitExprNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="NullLitExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        public NullLitExprNode(int line)
            : base(line, null, TypeCode.Empty) { }

        /// <summary>
        /// Returns the text representation of the null literal.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => "null";

        /// <summary>
        /// Determines whether the specified node is a null literal expression node.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns>true if the node is a null literal; otherwise, false.</returns>
        public override bool Equals([AllowNull] ASTNode other) => other is NullLitExprNode;
    }

    /// <summary>
    /// Represents a conditional (ternary) expression node.
    /// </summary>
    public sealed class CondExprNode : ExprNode
    {
        /// <summary>
        /// Gets the condition expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Condition => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Gets the then-expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode ThenExpression => this.Children[1].As<ExprNode>();

        /// <summary>
        /// Gets the else-expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode ElseExpression => this.Children[2].As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="CondExprNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="cond">The condition expression.</param>
        /// <param name="then">The then-expression.</param>
        /// <param name="else">The else-expression.</param>
        public CondExprNode(int line, ExprNode cond, ExprNode @then, ExprNode @else)
            : base(line, cond, @then, @else) { }

        /// <summary>
        /// Returns the text representation of the conditional expression.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"{this.Condition.GetText()} ? {this.ThenExpression.GetText()} : {this.ElseExpression.GetText()}";
    }
}
