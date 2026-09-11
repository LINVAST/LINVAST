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
using System.Collections;
using LINVAST.Exceptions;

namespace LINVAST.Imperative.Nodes.Common
{
    /// <summary>
    /// Provides static methods for evaluating unary operations on primitive types.
    /// </summary>
    public static class UnaryOperations
    {
        /// <summary>
        /// Gets the unary operation function for the specified symbol.
        /// </summary>
        /// <param name="symbol">The unary operator symbol (e.g., "+", "-", "!", "++").</param>
        /// <returns>A function that applies the unary operation to a value.</returns>
        /// <exception cref="UnknownOperatorException">Thrown when the symbol is not a recognized unary operator.</exception>
        public static Func<object, object> UnaryFromSymbol(string symbol)
        {
            return symbol switch
            {
                "+" => x => x,
                "-" => NegatePrimitive,
                "!" => NotPrimitive,
                "not" => NotPrimitive,
                "~" => BitwiseNotPrimitive,
                "#" => LengthPrimitive,
                "++" => IncrementPrimitive,
                "--" => DecrementPrimitive,
                "*" => x => x,  // TODO
                "&" => x => x,  // TODO
                _ => throw new UnknownOperatorException(symbol)
            };
        }

        /// <summary>
        /// Negates a primitive value.
        /// </summary>
        /// <param name="x">The value to negate.</param>
        /// <returns>The negated value.</returns>
        /// <exception cref="SyntaxErrorException">Thrown when attempting to negate a string.</exception>
        /// <exception cref="EvaluationException">Thrown when the value is not a primitive type.</exception>
        public static object NegatePrimitive(object x)
        {
            ThrowIfNotPrimitiveType(x);

            if (x is string)
                throw new SyntaxErrorException("Cannot negate strings");
            else if (x is decimal)
                return -Convert.ToDecimal(x);
            else if (x is double)
                return -Convert.ToDouble(x);
            else if (x is float)
                return -Convert.ToSingle(x);
            else if (x is ulong)
                return (ulong)(-Convert.ToInt64(x));
            else if (x is long)
                return -Convert.ToInt64(x);
            else if (x is uint)
                return -Convert.ToUInt32(x);
            else if (x is int)
                return -Convert.ToInt32(x);
            else if (x is ushort)
                return -Convert.ToUInt16(x);
            else if (x is short)
                return -Convert.ToInt16(x);
            else if (x is char)
                return -Convert.ToChar(x);
            else if (x is byte)
                return -Convert.ToByte(x);
            else if (x is sbyte)
                return -Convert.ToSByte(x);
            else
                throw new EvaluationException("Cannot negate non-primitive types");
        }

        /// <summary>
        /// Performs a bitwise NOT operation on a primitive value.
        /// </summary>
        /// <param name="x">The value to operate on.</param>
        /// <returns>The result of the bitwise NOT operation.</returns>
        /// <exception cref="EvaluationException">Thrown when the value is not a primitive type or is a floating-point type.</exception>
        public static object BitwiseNotPrimitive(object x)
        {
            ThrowIfNotPrimitiveType(x);

            if (x is string || x is decimal || x is double || x is float)
                throw new EvaluationException("Bitwise operations can't be performed on floating point numbers");
            else if (x is ulong)
                return ~Convert.ToUInt64(x);
            else if (x is long)
                return ~Convert.ToInt64(x);
            else if (x is uint)
                return ~Convert.ToUInt32(x);
            else if (x is int)
                return ~Convert.ToInt32(x);
            else if (x is ushort)
                return ~Convert.ToUInt16(x);
            else if (x is short)
                return ~Convert.ToInt16(x);
            else if (x is char)
                return ~Convert.ToChar(x);
            else if (x is byte)
                return ~Convert.ToByte(x);
            else if (x is sbyte)
                return ~Convert.ToSByte(x);
            else
                throw new EvaluationException("Cannot perform bitwise not on non-primitive types");
        }

        /// <summary>
        /// Increments a primitive value by one.
        /// </summary>
        /// <param name="x">The value to increment.</param>
        /// <returns>The incremented value.</returns>
        /// <exception cref="EvaluationException">Thrown when the value is not a primitive type.</exception>
        public static object IncrementPrimitive(object x)
        {
            ThrowIfNotPrimitiveType(x);

            if (x is string)
                throw new EvaluationException("Increment operation can't be performed on strings");
            else if (x is decimal)
                return Convert.ToDecimal(x) + 1m;
            else if (x is double)
                return Convert.ToDouble(x) + 1d;
            else if (x is float)
                return Convert.ToSingle(x) + 1f;
            else if (x is ulong)
                return Convert.ToUInt64(x) + 1uL;
            else if (x is long)
                return Convert.ToInt64(x) + 1L;
            else if (x is uint)
                return Convert.ToUInt32(x) + 1u;
            else if (x is int)
                return Convert.ToInt32(x) + 1;
            else if (x is ushort)
                return Convert.ToUInt16(x) + 1;
            else if (x is short)
                return Convert.ToInt16(x) + 1;
            else if (x is char)
                return Convert.ToChar(x) + 1;
            else if (x is byte)
                return Convert.ToByte(x) + 1;
            else if (x is sbyte)
                return Convert.ToSByte(x) + 1;
            else
                throw new EvaluationException("Cannot perform increment on non-primitive types");
        }

        /// <summary>
        /// Decrements a primitive value by one.
        /// </summary>
        /// <param name="x">The value to decrement.</param>
        /// <returns>The decremented value.</returns>
        /// <exception cref="EvaluationException">Thrown when the value is not a primitive type.</exception>
        public static object DecrementPrimitive(object x)
        {
            ThrowIfNotPrimitiveType(x);

            if (x is string)
                throw new EvaluationException("Decrement operation can't be performed on strings");
            else if (x is decimal)
                return Convert.ToDecimal(x) - 1m;
            else if (x is double)
                return Convert.ToDouble(x) - 1d;
            else if (x is float)
                return Convert.ToSingle(x) - 1f;
            else if (x is ulong)
                return Convert.ToUInt64(x) - 1uL;
            else if (x is long)
                return Convert.ToInt64(x) - 1L;
            else if (x is uint)
                return Convert.ToUInt32(x) - 1u;
            else if (x is int)
                return Convert.ToInt32(x) - 1;
            else if (x is ushort)
                return Convert.ToUInt16(x) - 1;
            else if (x is short)
                return Convert.ToInt16(x) - 1;
            else if (x is char)
                return Convert.ToChar(x) - 1;
            else if (x is byte)
                return Convert.ToByte(x) - 1;
            else if (x is sbyte)
                return Convert.ToSByte(x) - 1;
            else
                throw new EvaluationException("Cannot perform decrement on non-primitive types");
        }

        /// <summary>
        /// Performs a logical NOT operation on a primitive value.
        /// </summary>
        /// <param name="x">The value to operate on.</param>
        /// <returns>The result of the logical NOT operation.</returns>
        /// <exception cref="EvaluationException">Thrown when the value is not a primitive type.</exception>
        public static object NotPrimitive(object x)
        {
            ThrowIfNotPrimitiveType(x);

            if (x is string)
                throw new EvaluationException("Negate operation can't be performed on strings");
            else if (x is decimal)
                return !Convert.ToBoolean(Convert.ToDecimal(x));
            else if (x is double)
                return !Convert.ToBoolean(Convert.ToDouble(x));
            else if (x is float)
                return !Convert.ToBoolean(Convert.ToSingle(x));
            else if (x is ulong)
                return !Convert.ToBoolean(Convert.ToUInt64(x));
            else if (x is long)
                return !Convert.ToBoolean(Convert.ToInt64(x));
            else if (x is uint)
                return !Convert.ToBoolean(Convert.ToUInt32(x));
            else if (x is int)
                return !Convert.ToBoolean(Convert.ToInt32(x));
            else if (x is ushort)
                return !Convert.ToBoolean(Convert.ToUInt16(x));
            else if (x is short)
                return !Convert.ToBoolean(Convert.ToInt16(x));
            else if (x is char)
                return !Convert.ToBoolean(Convert.ToChar(x));
            else if (x is byte)
                return !Convert.ToBoolean(Convert.ToByte(x));
            else if (x is sbyte)
                return !Convert.ToBoolean(Convert.ToSByte(x));
            else if (x is bool)
                return !Convert.ToBoolean(x);
            else
                throw new EvaluationException("Cannot perform negate on non-primitive types");
        }

        /// <summary>
        /// Gets the length of a string or collection.
        /// </summary>
        /// <param name="x">The string or collection whose length to get.</param>
        /// <returns>The length of the string or count of the collection.</returns>
        /// <exception cref="EvaluationException">Thrown when the value does not have a length property.</exception>
        public static object LengthPrimitive(object x)
        {
            if (x is string str)
                return str.Length;
            if (x is ICollection collection)
                return collection.Count;

            throw new EvaluationException("Length operation can't be performed on this expression");
        }


        private static void ThrowIfNotPrimitiveType(object x)
        {
            Type tx = x.GetType();
            if (!tx.IsPrimitive && tx != typeof(decimal) && tx != typeof(string))
                throw new EvaluationException("Non-primitive type supplied to arithmetic operation!");
        }
    }
}
