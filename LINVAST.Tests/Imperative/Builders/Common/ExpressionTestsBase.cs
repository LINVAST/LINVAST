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
using System.Linq;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Visitors;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.Common
{
    internal abstract class ExpressionTestsBase : ASTBuilderTestBase
    {
        protected void AssertEvaluationException(string code)
        {
            ExprNode expr = this.AssertExpression(code);
            Assert.That(() => ConstantExpressionEvaluator.TryEvaluateAs(expr, out object result), Throws.InstanceOf<EvaluationException>());
        }

        protected void AssertExpressionValue<T>(string code, T expected)
        {
            ExprNode expr = this.AssertExpression(code);
            Assert.That(ConstantExpressionEvaluator.TryEvaluateAs(expr, out T result));
            Assert.That(result, Is.Not.Null);
            Assert.That(result, Is.EqualTo(expected).Within(1e-10));
        }

        protected void AssertNullExpression(string code)
        {
            ExprNode expr = this.AssertExpression(code);
            if (expr is NullLitExprNode @null) {
                Assert.That(@null.Value, Is.Null);
                Assert.That(@null.TypeCode, Is.EqualTo(TypeCode.Empty));
            } else {
                Assert.Fail("Initializer is not of type NullLiteralNode");
            }
        }

        protected ExprNode AssertExpression(string code)
        {
            ExprNode expr = this.GenerateAST(code).As<ExprNode>();
            Assert.That(expr, Is.Not.Null);
            return expr;
        }

        protected void AssertLiteralSuffix(string code, string? suffix, object value, Type type)
        {
            LitExprNode literal = this.GenerateAST(code).As<LitExprNode>();
            Assert.That(literal, Is.Not.Null);
            Assert.That(literal.Value?.GetType(), Is.EqualTo(type));
            Assert.That(literal.Suffix, Is.EqualTo(suffix));
            Assert.That(ConstantExpressionEvaluator.Evaluate(literal), Is.EqualTo(value).Within(1e-10));
        }

        protected void AssertFunctionCallExpression(string code, string fname, params object[] args)
        {
            FuncCallExprNode fcall = this.GenerateAST(code).As<FuncCallExprNode>();
            this.AssertChildrenParentProperties(fcall);
            Assert.That(fcall.Identifier, Is.EqualTo(fname));

            if (args is null || !args.Any()) {
                Assert.That(fcall.Arguments, Is.Null);
            } else {
                Assert.That(fcall.Arguments, Is.Not.Null);
                Assert.That(fcall.Arguments!.Expressions.Count, Is.EqualTo(args.Length));
                foreach ((ExprNode arg, object? expected) in fcall.Arguments!.Expressions.Zip(args))
                    Assert.That(ConstantExpressionEvaluator.Evaluate(arg), Is.EqualTo(expected).Within(1e-10));
            }
        }
    }
}
