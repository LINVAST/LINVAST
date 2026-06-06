using System;
using LINVAST.Builders;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;
using LINVAST.Nodes;

namespace LINVAST.Imperative.Builders.Kotlin
{
    public sealed partial class KotlinASTBuilder : KotlinParserBaseVisitor<ASTNode>, IASTBuilder<KotlinParser>
    {
        // Grammar rule: ifExpression : IF NL* LPAREN expression RPAREN NL* controlStructureBody?
        //                              SEMICOLON? (NL* ELSE NL* controlStructureBody?)?
        public override ASTNode VisitIfExpression(KotlinParser.IfExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode condition = this.Visit(ctx.expression()).As<ExprNode>();
            StatNode thenBody = this.Visit(ctx.controlStructureBody(0)).As<StatNode>();
            if (ctx.controlStructureBody().Length > 1) {
                StatNode elseBody = this.Visit(ctx.controlStructureBody(1)).As<StatNode>();
                return new IfStatNode(line, condition, thenBody, elseBody);
            }
            return new IfStatNode(line, condition, thenBody);
        }

        // Grammar rule: controlStructureBody : block | expression
        public override ASTNode VisitControlStructureBody(KotlinParser.ControlStructureBodyContext ctx)
        {
            int line = ctx.Start.Line;
            if(ctx.block() != null) return this.Visit(ctx.block());
            else return new ExprStatNode(line, this.Visit(ctx.expression()).As<ExprNode>());
        }

        // Grammar rule: whileExpression : WHILE NL* LPAREN expression RPAREN NL* controlStructureBody?
        public override ASTNode VisitWhileExpression(KotlinParser.WhileExpressionContext ctx)
        {
            int line = ctx.Start.Line;
            ExprNode condition = this.Visit(ctx.expression()).As<ExprNode>();
            StatNode body = ctx.controlStructureBody() != null 
                            ? this.Visit(ctx.controlStructureBody()).As<StatNode>()
                            : new BlockStatNode(line); // empty body   
            return new WhileStatNode(line, condition, body);
        }


        // Grammar rule: doWhileExpression : DO NL* controlStructureBody? NL* WHILE NL* LPAREN expression RPAREN
        public override ASTNode VisitDoWhileExpression(KotlinParser.DoWhileExpressionContext ctx)
        {
            throw new NotImplementedException("do-while is not supported");
        }


        // Grammar rule: forExpression : FOR NL* LPAREN annotations*
        //                               (variableDeclaration | multiVariableDeclaration)
        //                               IN expression RPAREN NL* controlStructureBody?
        public override ASTNode VisitForExpression(KotlinParser.ForExpressionContext ctx)
        {
            throw new NotImplementedException("for expression is not supported");
        }

        // Grammar rule: jumpExpression : THROW NL* expression
        //                              | (RETURN | RETURN_AT) expression?
        //                              | CONTINUE | CONTINUE_AT | BREAK | BREAK_AT
        public override ASTNode VisitJumpExpression(KotlinParser.JumpExpressionContext ctx)
        {
            int line = ctx.Start.Line; 
            if(ctx.RETURN() != null || ctx.RETURN_AT() != null) {
                if(ctx.expression() != null) { return new JumpStatNode(line, this.Visit(ctx.expression()).As<ExprNode>()); }
                return new JumpStatNode(line, JumpStatType.Return);
            }
            
            throw new NotImplementedException("break and continue are not supported");
        }

        // Grammar rule: expression : disjunction (assignmentOperator disjunction)*
        // TODO: support assignment operators (=, +=, -=, *=, /=, %=)
        public override ASTNode VisitExpression(KotlinParser.ExpressionContext ctx)
        {
            int line = ctx.Start.Line;   
            if(ctx.assignmentOperator().Length > 0) {
                ExprNode left = this.Visit(ctx.disjunction(0)).As<ExprNode>();
                AssignOpNode op = AssignOpNode.FromSymbol(line, ctx.assignmentOperator(0).GetText());
                ExprNode right = this.Visit(ctx.disjunction(1)).As<ExprNode>();
                return new ExprStatNode(line, new AssignExprNode(line, left, op, right));
            }
            
            return this.Visit(ctx.disjunction(0));
        }
    }
}
