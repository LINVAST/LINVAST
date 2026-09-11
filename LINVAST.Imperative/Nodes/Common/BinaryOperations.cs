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
using LINVAST.Exceptions;

namespace LINVAST.Imperative.Nodes.Common
{
    /// <summary>
    /// Provides primitive binary operations for arithmetic, relational, logical, and bitwise expressions.
    /// </summary>
    public static class BinaryOperations
    {
        /// <summary>
        /// Returns a binary arithmetic function for the given operator symbol.
        /// </summary>
        /// <param name="symbol">The symbol operand.</param>
        /// <returns>A delegate that performs the arithmetic operation.</returns>
        public static Func<object, object, object> ArithmeticFromSymbol(string symbol)
        {
            return symbol switch
            {
                "+" => AddPrimitive,
                "-" => SubtractPrimitive,
                "*" => MultiplyPrimitive,
                "/" => DividePrimitive,
                "<<" => ShiftLeftPrimitive,
                ">>" => ShiftRightPrimitive,
                ">>>" => ShiftRightPrimitive,
                "%" => ModulusPrimitive,
                "//" => FloorDividePrimitive,
                "^" => PowerPrimitive,
                "div" => FloorDividePrimitive,
                "mod" => ModulusPrimitive,
                _ => throw new UnknownOperatorException(symbol),
            };
        }

        /// <summary>
        /// Returns a binary relational function for the given operator symbol.
        /// </summary>
        /// <param name="symbol">The symbol operand.</param>
        /// <returns>A delegate that performs the relational comparison.</returns>
        public static Func<object, object, bool> RelationalFromSymbol(string symbol)
        {
            return symbol switch
            {
                ">" => GreaterThanPrimitive,
                "<" => LessThanPrimitive,
                ">=" => GreaterThanOrEqualPrimitive,
                "<=" => LessThanOrEqualPrimitive,
                "==" => EqualsPrimitive,
                "!=" => NotEqualsPrimitive,
                "~=" => NotEqualsPrimitive,
                "%=" => throw new NotImplementedException(symbol),
                "//=" => throw new NotImplementedException(symbol),
                _ => throw new UnknownOperatorException(symbol),
            };
        }

        /// <summary>
        /// Returns a binary assignment function for the given operator symbol.
        /// </summary>
        /// <param name="symbol">The symbol operand.</param>
        /// <returns>A delegate that performs the assignment operation.</returns>
        public static Func<object, object, object> AssignmentFromSymbol(string symbol)
        {
            return symbol switch
            {
                "=" => (a, b) => b,
                ":=" => (a, b) => b,
                "+=" => AddPrimitive,
                "-=" => SubtractPrimitive,
                "*=" => MultiplyPrimitive,
                "/=" => DividePrimitive,
                "%=" => ModulusPrimitive,
                "<<=" => ShiftLeftPrimitive,
                ">>=" => ShiftRightPrimitive,
                ">>>=" => ShiftRightPrimitive,
                "&=" => BitwiseAndPrimitive,
                "|=" => BitwiseOrPrimitive,
                "^=" => BitwiseXorPrimitive,
                _ => throw new UnknownOperatorException(symbol),
            };
        }

        /// <summary>
        /// Returns a binary bitwise function for the given operator symbol.
        /// </summary>
        /// <param name="symbol">The symbol operand.</param>
        /// <returns>A delegate that performs the bitwise operation.</returns>
        public static Func<object, object, object> BitwiseBinaryFromSymbol(string symbol)
        {
            return symbol switch
            {
                "<<" => ShiftLeftPrimitive,
                ">>" => ShiftRightPrimitive,
                "&" => BitwiseAndPrimitive,
                "|" => BitwiseOrPrimitive,
                "^" => BitwiseXorPrimitive,
                _ => throw new UnknownOperatorException(symbol),
            };
        }

        /// <summary>
        /// Returns a binary logical function for the given operator symbol.
        /// </summary>
        /// <param name="symbol">The symbol operand.</param>
        /// <returns>A delegate that performs the logical operation.</returns>
        public static Func<bool, bool, bool> LogicFromSymbol(string symbol)
        {
            return symbol switch
            {
                "&&" => (a, b) => a && b,
                "and" => (a, b) => a && b,
                "||" => (a, b) => a || b,
                "or" => (a, b) => a || b,
                _ => throw new UnknownOperatorException(symbol),
            };
        }


        /// <summary>
        /// Adds two primitive values.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The sum of x and y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when addition is not supported.</exception>
        public static object AddPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is string || y is string)
                return (string)x + (string)y;
            else if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) + Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) + Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) + Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) + Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) + Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) + Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) + Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) + Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) + Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) + Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) + Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) + Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot add non-primitive types");
        }

        /// <summary>
        /// Subtracts two primitive values.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The difference of x and y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when subtraction is not supported.</exception>
        public static object SubtractPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) - Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) - Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) - Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) - Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) - Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) - Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) - Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) - Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) - Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) - Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) - Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) - Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot subtract non-primitive types");
        }

        /// <summary>
        /// Multiplies two primitive values.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The product of x and y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when multiplication is not supported.</exception>
        public static object MultiplyPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) * Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) * Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) * Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) * Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) * Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) * Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) * Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) * Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) * Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) * Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) * Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) * Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot multiply non-primitive types");
        }

        /// <summary>
        /// Divides two primitive values.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The quotient of x divided by y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when division is not supported.</exception>
        public static object DividePrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) / Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) / Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) / Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) / Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) / Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) / Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) / Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) / Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) / Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) / Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) / Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) / Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot divide non-primitive types");
        }

        /// <summary>
        /// Divides two primitive values.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The quotient of x divided by y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when division is not supported.</exception>
        public static object FloorDividePrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is string || y is string)
                throw new EvaluationException("Cannot divide strings");
            else if (x is decimal || y is decimal)
                return Math.Floor(Convert.ToDecimal(x) / Convert.ToDecimal(y));
            else if (x is double || y is double)
                return Math.Floor(Convert.ToDouble(x) / Convert.ToDouble(y));
            else if (x is float || y is float)
                return (float)Math.Floor(Convert.ToSingle(x) / Convert.ToSingle(y));

            decimal result = Math.Floor(Convert.ToDecimal(x) / Convert.ToDecimal(y));
            if (x is ulong || y is ulong)
                return Convert.ToUInt64(result);
            else if (x is long || y is long)
                return Convert.ToInt64(result);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(result);
            else if (x is int || y is int)
                return Convert.ToInt32(result);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(result);
            else if (x is short || y is short)
                return Convert.ToInt16(result);
            else if (x is char || y is char)
                return Convert.ToChar(result);
            else if (x is byte || y is byte)
                return Convert.ToByte(result);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(result);
            else
                throw new EvaluationException("Cannot divide non-primitive types");
        }

        /// <summary>
        /// Raises x to the power of y.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>x raised to the power of y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when exponentiation is not supported.</exception>
        public static object PowerPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is string || y is string)
                throw new EvaluationException("Cannot exponentiate strings");

            double result = Math.Pow(Convert.ToDouble(x), Convert.ToDouble(y));
            bool isIntegralResult = Math.Abs(result % 1) < double.Epsilon;
            bool hasNegativeExponent = Convert.ToDouble(y) < 0;
            if (!isIntegralResult || hasNegativeExponent || x is decimal || y is decimal || x is double || y is double)
                return result;
            else if (x is float || y is float)
                return (float)result;
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(result);
            else if (x is long || y is long)
                return Convert.ToInt64(result);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(result);
            else if (x is int || y is int)
                return Convert.ToInt32(result);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(result);
            else if (x is short || y is short)
                return Convert.ToInt16(result);
            else if (x is char || y is char)
                return Convert.ToChar(result);
            else if (x is byte || y is byte)
                return Convert.ToByte(result);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(result);
            else
                throw new EvaluationException("Cannot exponentiate non-primitive types");
        }

        /// <summary>
        /// Computes the modulus of two primitive values.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The remainder of x divided by y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when modulus is not supported.</exception>
        public static object ModulusPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) % Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) % Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) % Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) % Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) % Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) % Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) % Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) % Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) % Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) % Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) % Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) % Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot divide non-primitive types");
        }

        /// <summary>
        /// Performs a left bitwise shift.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The result of shifting x left by y bits.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when shifting is not supported.</exception>
        public static object ShiftLeftPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (y is int || y is long || y is char || y is sbyte || y is byte)
                return x switch
                {
                    ulong _ => Convert.ToUInt64(x) << Convert.ToInt32(y),
                    long _ => Convert.ToInt64(x) << Convert.ToInt32(y),
                    uint _ => Convert.ToUInt32(x) << Convert.ToInt32(y),
                    int _ => Convert.ToInt32(x) << Convert.ToInt32(y),
                    ushort _ => Convert.ToUInt16(x) << Convert.ToInt16(y),
                    short _ => Convert.ToInt16(x) << Convert.ToInt16(y),
                    char _ => Convert.ToChar(x) << Convert.ToInt32(y),
                    sbyte _ => Convert.ToSByte(x) << Convert.ToInt32(y),
                    byte _ => Convert.ToByte(x) << Convert.ToInt32(y),
                    _ => throw new EvaluationException("Cannot shift non-integer types"),
                };
            else {
                throw new EvaluationException("Cannot shift by non-integer amount");
            }
        }

        /// <summary>
        /// Performs a right bitwise shift.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The result of shifting x right by y bits.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when shifting is not supported.</exception>
        public static object ShiftRightPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (y is int || y is long || y is char || y is sbyte || y is byte)
                return x switch
                {
                    ulong _ => Convert.ToUInt64(x) >> Convert.ToInt32(y),
                    long _ => Convert.ToInt64(x) >> Convert.ToInt32(y),
                    uint _ => Convert.ToUInt32(x) >> Convert.ToInt32(y),
                    int _ => Convert.ToInt32(x) >> Convert.ToInt32(y),
                    ushort _ => Convert.ToUInt16(x) >> Convert.ToInt16(y),
                    short _ => Convert.ToInt16(x) >> Convert.ToInt16(y),
                    char _ => Convert.ToChar(x) >> Convert.ToInt32(y),
                    sbyte _ => Convert.ToSByte(x) >> Convert.ToInt32(y),
                    byte _ => Convert.ToByte(x) >> Convert.ToInt32(y),
                    _ => throw new EvaluationException("Cannot shift non-integer types"),
                };
            else {
                throw new EvaluationException("Cannot shift by non-integer amount");
            }
        }

        /// <summary>
        /// Performs a bitwise AND operation.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The result of x AND y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when the operation is not supported.</exception>
        public static object BitwiseAndPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is string || y is string || x is decimal || y is decimal || x is double || y is double || x is float || y is float)
                throw new EvaluationException("Bitwise operations can't be performed on floating point numbers");
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) & Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) & Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) & Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) & Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) & Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) & Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) & Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) & Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) & Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot perform bitwise and on non-primitive types");
        }

        /// <summary>
        /// Performs a bitwise XOR operation.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The result of x XOR y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when the operation is not supported.</exception>
        public static object BitwiseXorPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is string || y is string || x is decimal || y is decimal || x is double || y is double || x is float || y is float)
                throw new EvaluationException("Bitwise operations can't be performed on floating point numbers");
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) ^ Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) ^ Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) ^ Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) ^ Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) ^ Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) ^ Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) ^ Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) ^ Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) ^ Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot perform bitwise xor on non-primitive types");
        }

        /// <summary>
        /// Performs a bitwise OR operation.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>The result of x OR y.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when the operation is not supported.</exception>
        public static object BitwiseOrPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is string || y is string || x is decimal || y is decimal || x is double || y is double || x is float || y is float)
                throw new EvaluationException("Bitwise operations can't be performed on floating point numbers");
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) | Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) | Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) | Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) | Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) | Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) | Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) | Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) | Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) | Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot perform bitwise or on non-primitive types");
        }

        /// <summary>
        /// Compares two primitive values using less-than.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>True if x is less than y; otherwise, false.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when comparison is not supported.</exception>
        public static bool LessThanPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) < Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) < Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) < Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) < Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) < Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) < Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) < Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) < Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) < Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) < Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) < Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) < Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot compare non-primitive types");
        }

        /// <summary>
        /// Compares two primitive values using less-than-or-equal.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>True if x is less than or equal to y; otherwise, false.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when comparison is not supported.</exception>
        public static bool LessThanOrEqualPrimitive(object x, object y)

        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) <= Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) <= Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) <= Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) <= Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) <= Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) <= Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) <= Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) <= Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) <= Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) <= Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) <= Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) <= Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot compare non-primitive types");
        }

        /// <summary>
        /// Compares two primitive values using greater-than.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>True if x is greater than y; otherwise, false.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when comparison is not supported.</exception>
        public static bool GreaterThanPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) > Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) > Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) > Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) > Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) > Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) > Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) > Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) > Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) > Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) > Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) > Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) > Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot compare non-primitive types");
        }

        /// <summary>
        /// Compares two primitive values using greater-than-or-equal.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>True if x is greater than or equal to y; otherwise, false.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type or when comparison is not supported.</exception>
        public static bool GreaterThanOrEqualPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            if (x is decimal || y is decimal)
                return Convert.ToDecimal(x) >= Convert.ToDecimal(y);
            else if (x is double || y is double)
                return Convert.ToDouble(x) >= Convert.ToDouble(y);
            else if (x is float || y is float)
                return Convert.ToSingle(x) >= Convert.ToSingle(y);
            else if (x is ulong || y is ulong)
                return Convert.ToUInt64(x) >= Convert.ToUInt64(y);
            else if (x is long || y is long)
                return Convert.ToInt64(x) >= Convert.ToInt64(y);
            else if (x is uint || y is uint)
                return Convert.ToUInt32(x) >= Convert.ToUInt32(y);
            else if (x is int || y is int)
                return Convert.ToInt32(x) >= Convert.ToInt32(y);
            else if (x is ushort || y is ushort)
                return Convert.ToUInt16(x) >= Convert.ToUInt16(y);
            else if (x is short || y is short)
                return Convert.ToInt16(x) >= Convert.ToInt16(y);
            else if (x is char || y is char)
                return Convert.ToChar(x) >= Convert.ToChar(y);
            else if (x is byte || y is byte)
                return Convert.ToByte(x) >= Convert.ToByte(y);
            else if (x is sbyte || y is sbyte)
                return Convert.ToSByte(x) >= Convert.ToSByte(y);
            else
                throw new EvaluationException("Cannot compare non-primitive types");
        }

        /// <summary>
        /// Compares two primitive values for equality.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>True if x equals y; otherwise, false.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type.</exception>
        public static bool EqualsPrimitive(object x, object y)
        {
            ThrowIfNotPrimitiveTypes(x, y);

            return x.Equals(y);
        }

        /// <summary>
        /// Compares two primitive values for equality.
        /// </summary>
        /// <param name="x">The x operand.</param>
        /// <param name="y">The y operand.</param>
        /// <returns>True if x equals y; otherwise, false.</returns>
        /// <exception cref="EvaluationException">Thrown when either operand is not a primitive type.</exception>
        public static bool NotEqualsPrimitive(object x, object y)
            => !EqualsPrimitive(x, y);


        private static void ThrowIfNotPrimitiveTypes(object x, object y)
        {
            if (!IsPrimitiveType(x.GetType()) || !IsPrimitiveType(y.GetType()))
                throw new EvaluationException("Non-primitive type supplied to arithmetic operation!");
        }

        private static bool IsPrimitiveType(Type tx)
            => tx.IsPrimitive || tx == typeof(decimal) || tx == typeof(string);
    }
}
