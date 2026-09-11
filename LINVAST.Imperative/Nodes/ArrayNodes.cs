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
    /// <summary>Represents an array declaration node in the abstract syntax tree.</summary>
    public sealed class ArrDeclNode : DeclNode
    {
        /// <summary>Gets the array size expression, if specified.</summary>
        [JsonIgnore]
        public ExprNode? SizeExpression
            => this.Children.Count > 2 ? this.Children[1].As<ExprNode>()
                                       : this.Initializer is not null ? null
                                                                     : this.Children.ElementAtOrDefault(1) as ExprNode;

        /// <summary>Gets the array initializer expression, if specified.</summary>
        [JsonIgnore]
        public ArrInitExprNode? Initializer
            => this.Children.Count > 2 ? this.Children[2].As<ArrInitExprNode>()
                                       : this.Children.ElementAtOrDefault(1) as ArrInitExprNode;


        /// <summary>Initializes a new instance of the <see cref="ArrDeclNode"/> class with an identifier.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier for the array.</param>
        public ArrDeclNode(int line, IdNode identifier)
            : base(line, identifier) { }

        /// <summary>Initializes a new instance of the <see cref="ArrDeclNode"/> class with an identifier and size expression.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier for the array.</param>
        /// <param name="sizeExpr">The expression specifying the array size.</param>
        public ArrDeclNode(int line, IdNode identifier, ExprNode sizeExpr)
            : base(line, identifier, sizeExpr) { }

        /// <summary>Initializes a new instance of the <see cref="ArrDeclNode"/> class with an identifier and initializer.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier for the array.</param>
        /// <param name="init">The array initializer expression.</param>
        public ArrDeclNode(int line, IdNode identifier, ArrInitExprNode init)
            : base(line, identifier, init) { }

        /// <summary>Initializes a new instance of the <see cref="ArrDeclNode"/> class with an identifier, size expression, and initializer.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="identifier">The identifier for the array.</param>
        /// <param name="sizeExpr">The expression specifying the array size.</param>
        /// <param name="init">The array initializer expression.</param>
        public ArrDeclNode(int line, IdNode identifier, ExprNode sizeExpr, ArrInitExprNode init)
            : base(line, identifier, sizeExpr, init) { }


        /// <summary>Returns the text representation of the array declaration.</summary>
        /// <returns>The text representation of the node.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder(base.GetText());
            sb.Append('[').Append(this.SizeExpression?.ToString() ?? "").Append(']');
            if (this.Initializer is not null)
                sb.Append(" = ").Append(this.Initializer.ToString());
            return sb.ToString();
        }
    }

    /// <summary>Represents an array initializer expression node in the abstract syntax tree.</summary>
    public sealed class ArrInitExprNode : ExprListNode
    {
        /// <summary>Gets the initializer expressions.</summary>
        [JsonIgnore]
        public IEnumerable<ExprNode> Initializers => this.Expressions;


        /// <summary>Initializes a new instance of the <see cref="ArrInitExprNode"/> class with an enumerable of expressions.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="exprs">The initializer expressions.</param>
        public ArrInitExprNode(int line, IEnumerable<ExprNode> exprs)
            : base(line, exprs) { }

        /// <summary>Initializes a new instance of the <see cref="ArrInitExprNode"/> class with a params array of expressions.</summary>
        /// <param name="line">The source line number.</param>
        /// <param name="exprs">The initializer expressions.</param>
        public ArrInitExprNode(int line, params ExprNode[] exprs)
            : base(line, exprs) { }


        /// <summary>Returns the text representation of the array initializer.</summary>
        /// <returns>The text representation of the node.</returns>
        public override string GetText() => $"{{ {string.Join(", ", this.Initializers.Select(i => i.GetText()))} }}";
    }
}
