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


// LINVAST - Language-INariant AST library
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
using System.Diagnostics.CodeAnalysis;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Base class for all operator nodes in the abstract syntax tree.
    /// </summary>
    public abstract class OpNode : ASTNode
    {
        /// <summary>
        /// Gets the operator symbol.
        /// </summary>
        public string Symbol { get; }


        /// <summary>
        /// Initializes a new instance of the <see cref="OpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        protected OpNode(int line, string symbol)
            : base(line)
        {
            this.Symbol = symbol;
        }


        /// <summary>
        /// Returns the text representation of the operator.
        /// </summary>
        /// <returns>The operator symbol.</returns>
        public override string GetText() => this.Symbol;

        /// <summary>
        /// Determines whether the specified node is equal to this operator node.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns><c>true</c> if the nodes are equal; otherwise, <c>false</c>.</returns>
        public override bool Equals([AllowNull] ASTNode other)
            => base.Equals(other) && this.Symbol.Equals((other as OpNode)?.Symbol);
    }

    /// <summary>
    /// Represents a unary operator node in the abstract syntax tree.
    /// </summary>
    public sealed class UnaryOpNode : OpNode
    {
        /// <summary>
        /// Creates a unary operator node from the specified symbol.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <returns>A new <see cref="UnaryOpNode"/> instance.</returns>
        public static UnaryOpNode FromSymbol(int line, string symbol)
            => new(line, symbol, UnaryOperations.UnaryFromSymbol(symbol));


        /// <summary>
        /// Gets or sets the function that applies this unary operator to an operand.
        /// </summary>
        [JsonIgnore]
        public Func<object, object> ApplyTo { get; set; }


        /// <summary>
        /// Initializes a new instance of the <see cref="UnaryOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        public UnaryOpNode(int line, string symbol, Func<object, object> logic)
            : base(line, symbol)
        {
            this.ApplyTo = logic;
        }
    }

    /// <summary>
    /// Base class for binary operator nodes in the abstract syntax tree.
    /// </summary>
    public abstract class BinaryOpNode : OpNode
    {
        /// <summary>
        /// Gets or sets the function that applies this binary operator to two operands.
        /// </summary>
        [JsonIgnore]
        public Func<object, object, object> ApplyTo { get; set; }


        /// <summary>
        /// Initializes a new instance of the <see cref="BinaryOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        protected BinaryOpNode(int line, string symbol, Func<object, object, object> logic)
            : base(line, symbol)
        {
            this.ApplyTo = logic;
        }
    }

    /// <summary>
    /// Represents an arithmetic binary operator node in the abstract syntax tree.
    /// </summary>
    public sealed class ArithmOpNode : BinaryOpNode
    {
        /// <summary>
        /// Creates an arithmetic operator node from the specified symbol.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <returns>A new <see cref="ArithmOpNode"/> instance.</returns>
        public static ArithmOpNode FromSymbol(int line, string symbol)
            => new(line, symbol, BinaryOperations.ArithmeticFromSymbol(symbol));

        /// <summary>
        /// Creates an arithmetic operator node from a bitwise symbol.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <returns>A new <see cref="ArithmOpNode"/> instance.</returns>
        public static ArithmOpNode FromBitwiseSymbol(int line, string symbol)
            => new(line, symbol, BinaryOperations.BitwiseBinaryFromSymbol(symbol));


        /// <summary>
        /// Initializes a new instance of the <see cref="ArithmOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        public ArithmOpNode(int line, string symbol, Func<object, object, object> logic)
            : base(line, symbol, logic) { }
    }

    /// <summary>
    /// Represents a relational (comparison) binary operator node in the abstract syntax tree.
    /// </summary>
    public sealed class RelOpNode : BinaryOpNode
    {
        /// <summary>
        /// Creates a relational operator node from the specified symbol.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <returns>A new <see cref="RelOpNode"/> instance.</returns>
        public static RelOpNode FromSymbol(int line, string symbol)
            => new(line, symbol, BinaryOperations.RelationalFromSymbol(symbol));


        /// <summary>
        /// Initializes a new instance of the <see cref="RelOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        public RelOpNode(int line, string symbol, Func<object, object, bool> logic)
            : base(line, symbol, (x, y) => logic(x, y)) { }
    }

    /// <summary>
    /// Represents a binary logical operator node in the abstract syntax tree.
    /// </summary>
    public sealed class BinaryLogicOpNode : BinaryOpNode
    {
        /// <summary>
        /// Creates a binary logic operator node from the specified symbol.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <returns>A new <see cref="BinaryLogicOpNode"/> instance.</returns>
        public static BinaryLogicOpNode FromSymbol(int line, string symbol)
            => new(line, symbol, BinaryOperations.LogicFromSymbol(symbol));


        /// <summary>
        /// Initializes a new instance of the <see cref="BinaryLogicOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        public BinaryLogicOpNode(int line, string symbol, Func<bool, bool, bool> logic)
            : base(line, symbol, (x, y) => logic(Convert.ToBoolean(x), Convert.ToBoolean(y))) { }
    }

    /// <summary>
    /// Represents an assignment operator node in the abstract syntax tree.
    /// </summary>
    public class AssignOpNode : BinaryOpNode
    {
        /// <summary>
        /// Creates an assignment operator node from the specified symbol.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <returns>A new <see cref="AssignOpNode"/> or <see cref="ComplexAssignOpNode"/> instance.</returns>
        public static AssignOpNode FromSymbol(int line, string symbol)
        {
            return symbol == "=" || symbol == ":="
                ? new AssignOpNode(line, symbol, (a, b) => b)
                : new ComplexAssignOpNode(line, symbol, BinaryOperations.AssignmentFromSymbol(symbol));
        }


        /// <summary>
        /// Initializes a new instance of the <see cref="AssignOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        public AssignOpNode(int line, string symbol, Func<object, object, object> logic)
            : base(line, symbol, logic) { }
    }

    /// <summary>
    /// Represents a compound assignment operator node in the abstract syntax tree.
    /// </summary>
    public sealed class ComplexAssignOpNode : AssignOpNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ComplexAssignOpNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="symbol">The operator symbol.</param>
        /// <param name="logic">The function that applies this operator.</param>
        public ComplexAssignOpNode(int line, string symbol, Func<object, object, object> logic)
            : base(line, symbol, logic) { }
    }
}
