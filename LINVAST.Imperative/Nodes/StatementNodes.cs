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
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes
{
    /// <summary>
    /// Base class for all statement nodes in the abstract syntax tree.
    /// </summary>
    public abstract class StatNode : ASTNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="StatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected StatNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="StatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected StatNode(int line, params ASTNode[] children)
            : base(line, children) { }

        /// <summary>
        /// Returns the text representation of the statement.
        /// </summary>
        /// <returns>The text representation ending with a semicolon.</returns>
        public override string GetText() => $"{base.GetText()};";
    }

    /// <summary>
    /// Represents an empty statement node.
    /// </summary>
    public sealed class EmptyStatNode : StatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="EmptyStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        public EmptyStatNode(int line)
            : base(line) { }
    }

    /// <summary>
    /// Base class for simple statement nodes.
    /// </summary>
    public abstract class SimpleStatNode : StatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="SimpleStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected SimpleStatNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="SimpleStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected SimpleStatNode(int line, params ASTNode[] children)
            : base(line, children) { }
    }

    /// <summary>
    /// Represents a declaration statement node.
    /// </summary>
    public class DeclStatNode : SimpleStatNode
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
        /// Gets the declaration specifiers.
        /// </summary>
        [JsonIgnore]
        public DeclSpecsNode Specifiers => this.ChildrenWithoutTags.ElementAt(0).As<DeclSpecsNode>();

        /// <summary>
        /// Gets the declarator list.
        /// </summary>
        [JsonIgnore]
        public DeclListNode DeclaratorList => this.ChildrenWithoutTags.ElementAt(1).As<DeclListNode>();

        /// <summary>
        /// Gets the modifiers from the declaration specifiers.
        /// </summary>
        [JsonIgnore]
        public Modifiers Modifiers => this.Specifiers.Modifiers;

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="declSpecs">The declaration specifiers.</param>
        /// <param name="declList">The declarator list.</param>
        public DeclStatNode(int line, DeclSpecsNode declSpecs, DeclListNode declList)
            : base(line, declSpecs, declList) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="DeclStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="tags">The leading tags.</param>
        /// <param name="declSpecs">The declaration specifiers.</param>
        /// <param name="declList">The declarator list.</param>
        public DeclStatNode(int line, IEnumerable<TagNode> tags, DeclSpecsNode declSpecs, DeclListNode declList)
            : base(line, tags.Concat(new ASTNode[] { declSpecs, declList })) { }
    }

    /// <summary>
    /// Base class for complex statement nodes.
    /// </summary>
    public abstract class ComplexStatNode : StatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ComplexStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected ComplexStatNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ComplexStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected ComplexStatNode(int line, params ASTNode[] children)
            : base(line, children) { }
    }

    /// <summary>
    /// Represents a block statement node.
    /// </summary>
    public sealed class BlockStatNode : ComplexStatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="BlockStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        public BlockStatNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="BlockStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        public BlockStatNode(int line, params ASTNode[] children)
            : base(line, children) { }

        /// <summary>
        /// Returns the text representation of the block statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{{ {string.Join(" ", this.Children.Select(c => c.GetText()))} }}";
    }

    /// <summary>
    /// Represents an expression statement node.
    /// </summary>
    public class ExprStatNode : SimpleStatNode
    {
        /// <summary>
        /// Gets the expression of the statement.
        /// </summary>
        [JsonIgnore]
        public ExprNode Expression => this.Children.First().As<ExprNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="ExprStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="expr">The expression.</param>
        public ExprStatNode(int line, ExprNode expr)
            : base(line, expr) { }
    }

    /// <summary>
    /// Represents an if statement node.
    /// </summary>
    public sealed class IfStatNode : ComplexStatNode
    {
        /// <summary>
        /// Gets the condition expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Condition => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Gets the then-branch statement.
        /// </summary>
        [JsonIgnore]
        public StatNode ThenStat => this.Children[1].As<StatNode>();

        /// <summary>
        /// Gets the else-branch statement, if present.
        /// </summary>
        [JsonIgnore]
        public StatNode? ElseStat => this.Children.ElementAtOrDefault(2)?.As<StatNode>() ?? null;

        /// <summary>
        /// Initializes a new instance of the <see cref="IfStatNode"/> class without an else branch.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="cond">The condition expression.</param>
        /// <param name="then">The then-branch statement.</param>
        public IfStatNode(int line, ExprNode cond, StatNode @then)
            : base(line, cond, @then) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="IfStatNode"/> class with an else branch.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="cond">The condition expression.</param>
        /// <param name="then">The then-branch statement.</param>
        /// <param name="else">The else-branch statement.</param>
        public IfStatNode(int line, ExprNode cond, StatNode @then, StatNode @else)
            : base(line, cond, @then, @else) { }

        /// <summary>
        /// Returns the text representation of the if statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"if {this.Condition.GetText()} {this.ThenStat.GetText()} {(this.ElseStat is null ? "" : $"else {this.ElseStat.GetText()}")}";
    }

    /// <summary>
    /// Represents a switch statement node.
    /// </summary>
    public sealed class SwitchStatNode : ComplexStatNode
    {
        /// <summary>
        /// Gets the condition expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Condition => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Gets the body block of the switch statement.
        /// </summary>
        [JsonIgnore]
        public BlockStatNode Body => this.Children[1].As<BlockStatNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="SwitchStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="cond">The condition expression.</param>
        /// <param name="body">The body block.</param>
        public SwitchStatNode(int line, ExprNode cond, BlockStatNode body)
            : base(line, cond, body) { }

        /// <summary>
        /// Returns the text representation of the switch statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"switch {this.Condition.GetText()} {this.Body.GetText()}";
    }

    /// <summary>
    /// Represents a jump statement node (return, break, continue, goto).
    /// </summary>
    public sealed class JumpStatNode : SimpleStatNode
    {
        /// <summary>
        /// Gets or sets the type of jump statement.
        /// </summary>
        public JumpStatType Type { get; set; }

        /// <summary>
        /// Gets the return expression, if the type is Return.
        /// </summary>
        [JsonIgnore]
        public ExprNode? ReturnExpr => this.Children.FirstOrDefault() as ExprNode ?? null;

        /// <summary>
        /// Gets the goto label, if the type is Goto.
        /// </summary>
        [JsonIgnore]
        public IdNode? GotoLabel => this.Children.First() as IdNode ?? null;

        /// <summary>
        /// Initializes a new instance of the <see cref="JumpStatNode"/> class for break or continue.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="type">The jump statement type.</param>
        public JumpStatNode(int line, JumpStatType type)
            : base(line)
        {
            this.Type = type;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="JumpStatNode"/> class for return statements.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="returnExpr">The return expression, or null for void return.</param>
        public JumpStatNode(int line, ExprNode? returnExpr)
            : base(line, returnExpr is null ? Enumerable.Empty<ASTNode>() : new[] { returnExpr })
        {
            this.Type = JumpStatType.Return;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="JumpStatNode"/> class for goto statements.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="label">The goto label identifier.</param>
        public JumpStatNode(int line, IdNode label)
            : base(line, label)
        {
            this.Type = JumpStatType.Goto;
        }

        /// <summary>
        /// Returns the text representation of the jump statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder(this.Type.ToStringToken());
            if (this.Type == JumpStatType.Return && this.ReturnExpr is not null)
                sb.Append(' ').Append(this.ReturnExpr.GetText());
            else if (this.Type == JumpStatType.Goto && this.GotoLabel is not null)
                sb.Append(' ').Append(this.GotoLabel.GetText());
            sb.Append(';');
            return sb.ToString();
        }
    }

    /// <summary>
    /// Represents a labeled statement node.
    /// </summary>
    public sealed class LabeledStatNode : SimpleStatNode
    {
        /// <summary>
        /// Gets the label string.
        /// </summary>
        public string Label { get; }

        /// <summary>
        /// Gets the labeled statement.
        /// </summary>
        [JsonIgnore]
        public StatNode Statement => this.Children.First().As<StatNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="LabeledStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="label">The label string.</param>
        /// <param name="statement">The labeled statement.</param>
        public LabeledStatNode(int line, string label, StatNode statement)
            : base(line, statement)
        {
            this.Label = label;
        }

        /// <summary>
        /// Returns the text representation of the labeled statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"{this.Label}: {this.Statement.GetText()}";

        /// <summary>
        /// Determines whether the specified node is equal to the current labeled statement.
        /// </summary>
        /// <param name="other">The node to compare.</param>
        /// <returns>true if the nodes are equal; otherwise, false.</returns>
        public override bool Equals([AllowNull] ASTNode other)
            => base.Equals(other) && this.Label.Equals((other as LabeledStatNode)?.Label);
    }

    /// <summary>
    /// Base class for iterative statement nodes (loops).
    /// </summary>
    public abstract class IterStatNode : ComplexStatNode
    {
        /// <summary>
        /// Gets the loop condition expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Condition => this.Children[0].As<ExprNode>();

        /// <summary>
        /// Gets the loop body statement.
        /// </summary>
        [JsonIgnore]
        public StatNode Statement => this.Children[1].As<StatNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="IterStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="cond">The loop condition.</param>
        /// <param name="stat">The loop body statement.</param>
        protected IterStatNode(int line, ExprNode cond, StatNode stat)
            : base(line, cond, stat) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="IterStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="children">The child nodes.</param>
        protected IterStatNode(int line, IEnumerable<ASTNode> children)
            : base(line, children) { }
    }

    /// <summary>
    /// Represents a while statement node.
    /// </summary>
    public sealed class WhileStatNode : IterStatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="WhileStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="cond">The loop condition.</param>
        /// <param name="stat">The loop body statement.</param>
        public WhileStatNode(int line, ExprNode cond, StatNode stat)
            : base(line, cond, stat) { }

        /// <summary>
        /// Returns the text representation of the while statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"while {this.Condition.GetText()} {{ {this.Statement.GetText()} }}";
    }

    /// <summary>
    /// Represents a for statement node.
    /// </summary>
    public sealed class ForStatNode : IterStatNode
    {
        /// <summary>
        /// Gets the for-loop declaration, if present.
        /// </summary>
        public DeclarationNode? ForDeclaration { get; }

        /// <summary>
        /// Gets the initialization expression, if present.
        /// </summary>
        public ExprNode? InitExpr { get; }

        /// <summary>
        /// Gets the increment expression, if present.
        /// </summary>
        public ExprNode? IncrExpr { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="ForStatNode"/> class with a declaration initializer.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="decl">The loop variable declaration.</param>
        /// <param name="cond">The loop condition, or null for true.</param>
        /// <param name="expr">The increment expression.</param>
        /// <param name="stat">The loop body statement.</param>
        public ForStatNode(int line, DeclarationNode decl, ExprNode? cond, ExprNode? expr, StatNode stat)
            : base(line, new ASTNode[] { cond ?? new LitExprNode(line, true), stat })
        {
            this.ForDeclaration = decl;
            this.InitExpr = null;
            this.IncrExpr = expr;
        }

        /// <summary>
        /// Initializes a new instance of the <see cref="ForStatNode"/> class with an expression initializer.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="initExpr">The initialization expression.</param>
        /// <param name="cond">The loop condition, or null for true.</param>
        /// <param name="incExpr">The increment expression.</param>
        /// <param name="stat">The loop body statement.</param>
        public ForStatNode(int line, ExprNode? initExpr, ExprNode? cond, ExprNode? incExpr, StatNode stat)
            : base(line, new ASTNode[] { cond ?? new LitExprNode(line, true), stat })
        {
            this.ForDeclaration = null;
            this.InitExpr = initExpr;
            this.IncrExpr = incExpr;
        }

        /// <summary>
        /// Returns the text representation of the for statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
        {
            var sb = new StringBuilder("for (");
            if (this.ForDeclaration is not null)
                sb.Append(this.ForDeclaration.GetText());
            else if (this.InitExpr is not null)
                sb.Append(this.InitExpr.GetText());
            sb.Append("; ");
            sb.Append(this.Condition.GetText());
            sb.Append("; ");
            if (this.IncrExpr is not null)
                sb.Append(this.IncrExpr.GetText());
            sb.Append(") { ");
            sb.Append(this.Statement.GetText());
            sb.Append(" }");
            return sb.ToString();
        }
    }

    /// <summary>
    /// Represents a foreach statement node.
    /// </summary>
    public sealed class ForeachStatNode : ComplexStatNode
    {
        /// <summary>
        /// Gets the iterator declaration statement.
        /// </summary>
        [JsonIgnore]
        public DeclStatNode IteratorDeclaration => this.Children[0].As<DeclStatNode>();

        /// <summary>
        /// Gets the iterator declaration specifiers.
        /// </summary>
        [JsonIgnore]
        public DeclSpecsNode IteratorSpecifiers => this.IteratorDeclaration.Specifiers;

        /// <summary>
        /// Gets the iterator type.
        /// </summary>
        [JsonIgnore]
        public TypeNameNode IteratorType => this.IteratorSpecifiers.TypeNode;

        /// <summary>
        /// Gets the iterator declarator.
        /// </summary>
        [JsonIgnore]
        public DeclNode IteratorDeclarator => this.IteratorDeclaration.DeclaratorList.Declarators.Single();

        /// <summary>
        /// Gets the iterator identifier node.
        /// </summary>
        [JsonIgnore]
        public IdNode Iterator => this.IteratorDeclarator.IdentifierNode;

        /// <summary>
        /// Gets the iterable expression.
        /// </summary>
        [JsonIgnore]
        public ExprNode Iterable => this.Children[1].As<ExprNode>();

        /// <summary>
        /// Gets the loop body statement.
        /// </summary>
        [JsonIgnore]
        public StatNode Statement => this.Children[2].As<StatNode>();

        /// <summary>
        /// Initializes a new instance of the <see cref="ForeachStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="iteratorDeclaration">The iterator declaration statement.</param>
        /// <param name="iterable">The iterable expression.</param>
        /// <param name="stat">The loop body statement.</param>
        public ForeachStatNode(int line, DeclStatNode iteratorDeclaration, ExprNode iterable, StatNode stat)
            : base(line, iteratorDeclaration, iterable, stat) { }

        /// <summary>
        /// Initializes a new instance of the <see cref="ForeachStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="iteratorType">The iterator type.</param>
        /// <param name="iterator">The iterator identifier.</param>
        /// <param name="iterable">The iterable expression.</param>
        /// <param name="stat">The loop body statement.</param>
        public ForeachStatNode(int line, TypeNameNode iteratorType, IdNode iterator, ExprNode iterable, StatNode stat)
            : this(
                line,
                new DeclStatNode(
                    line,
                    new DeclSpecsNode(iteratorType.Line, iteratorType),
                    new DeclListNode(iterator.Line, new VarDeclNode(iterator.Line, iterator))),
                iterable,
                stat) { }

        /// <summary>
        /// Returns the text representation of the foreach statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText()
            => $"foreach ({this.IteratorSpecifiers.GetText()} {this.IteratorDeclarator.GetText()} : {this.Iterable.GetText()}) {{ {this.Statement.GetText()} }}";
    }

    /// <summary>
    /// Represents a throw statement node.
    /// </summary>
    public sealed class ThrowStatNode : ExprStatNode
    {
        /// <summary>
        /// Initializes a new instance of the <see cref="ThrowStatNode"/> class.
        /// </summary>
        /// <param name="line">The source line number.</param>
        /// <param name="exp">The exception expression.</param>
        public ThrowStatNode(int line, ExprNode exp)
            : base(line, exp) { }

        /// <summary>
        /// Returns the text representation of the throw statement.
        /// </summary>
        /// <returns>The text representation.</returns>
        public override string GetText() => $"throw {this.Expression.GetText()}";
    }
}
