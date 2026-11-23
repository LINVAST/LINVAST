using System;
using System.Collections;
using System.Numerics;
using LINVAST.Exceptions;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Nodes.Common;

namespace LINVAST.Imperative.Visitors
{
    internal static class SemanticEvaluation
    {
        internal static void Validate(ExprNode node)
        {
            if (node.Semantics is not { } s) return;
            bool valid = node switch {
                LogicExprNode => s.Truthiness is not null && s.LogicalResult is not null,
                CondExprNode => s.Truthiness is not null && s.LogicalResult is null,
                UnaryExprNode u => u.Operator.Symbol is "!" or "not" && s.Truthiness is not null && s.LogicalResult is null,
                BinaryExprNode b => Matches(b.Operator.Symbol.TrimEnd('='), s),
                _ => false,
            };
            if (!valid) throw new EvaluationException("Expression semantics do not match the operation.");
        }

        private static bool Matches(string op, ExpressionSemantics s) => op switch {
            "/" or "//" => s.Division is not null,
            "%" => s.Remainder is not null,
            "<<" or ">>" or ">>>" => s.RightShift is not null,
            _ => false,
        };

        internal static bool Truth(object? value, TruthinessMode mode)
        {
            if (mode == TruthinessMode.Lua) return value is not null && value is not false;
            if (value is bool boolean) return boolean;
            if (mode == TruthinessMode.BooleanOnly)
                throw new EvaluationException("This condition requires a Boolean operand.");
            if (mode == TruthinessMode.Python) {
                if (value is null) return false;
                if (value is string text) return text.Length != 0;
                if (value is ICollection collection) return collection.Count != 0;
            }
            if (IsInteger(value)) return Integer(value!) != 0;
            if (value is double d) return d != 0;
            if (value is float f) return f != 0;
            if (value is decimal m) return m != 0;
            throw new EvaluationException("Truthiness is unsupported for this value; runtime type behavior is required.");
        }

        internal static object Arithmetic(ArithmExprNode node, object x, object y)
        {
            ExpressionSemantics s = node.Semantics!;
            if (s.RightShift is not null) return Shift(node.Operator.Symbol, x, y, s);
            if (!IsNumber(x) || !IsNumber(y))
                throw new EvaluationException("Numeric semantics require numeric operands.");
            if (s.Division == DivisionMode.True) {
                double divisor = Convert.ToDouble(y);
                if (divisor == 0 && s.ZeroDivision == ZeroDivisionMode.Throw) throw new DivideByZeroException();
                return Convert.ToDouble(x) / divisor;
            }
            if (IsInteger(x) && IsInteger(y)) {
                BigInteger a = Integer(x), b = Integer(y);
                if (b.IsZero) throw new DivideByZeroException();
                BigInteger q = BigInteger.DivRem(a, b, out BigInteger r);
                if (s.Division == DivisionMode.Floor || s.Remainder == RemainderMode.DivisorSign) {
                    if (!r.IsZero && r.Sign != b.Sign) { q--; r += b; }
                }
                return Narrow(s.Remainder is not null ? r : q, x, y);
            }
            if (x is decimal || y is decimal) {
                decimal a = Convert.ToDecimal(x), b = Convert.ToDecimal(y);
                decimal r = a % b;
                if (s.Remainder == RemainderMode.DivisorSign && r != 0 && Math.Sign(r) != Math.Sign(b)) r += b;
                return s.Remainder is not null ? r : s.Division == DivisionMode.Floor ? Math.Floor(a / b) : a / b;
            }
            double left = Convert.ToDouble(x), right = Convert.ToDouble(y);
            if (right == 0 && s.ZeroDivision == ZeroDivisionMode.Throw) throw new DivideByZeroException();
            double remainder = left % right;
            double quotient = (left - remainder) / right;
            if (s.Division == DivisionMode.Floor || s.Remainder == RemainderMode.DivisorSign) {
                if (remainder != 0 && (remainder < 0) != (right < 0)) { remainder += right; quotient--; }
                if (remainder == 0) remainder = Math.CopySign(0, right);
            }
            // Derive floor division from the remainder to avoid rounding 1.0 / 0.1 up to 10.
            double floor = Math.Floor(quotient);
            if (quotient - floor > 0.5) floor++;
            if (floor == 0) floor = Math.CopySign(0, left / right);
            double result = s.Remainder is not null ? remainder : s.Division == DivisionMode.Floor ? floor : left / right;
            if (x is float && y is not double || y is float && x is not double) return (float)result;
            return result;
        }

        private static object Shift(string op, object x, object y, ExpressionSemantics s)
        {
            if (!IsInteger(x) || !IsInteger(y)) throw new EvaluationException("Shift operands must be integers.");
            BigInteger value = Integer(x), count = Integer(y);
            bool left = op == "<<";
            if (s.ShiftCount == ShiftCountMode.Unbounded) {
                if (count < 0) throw new EvaluationException("Shift count must be non-negative.");
                if (count > 63) {
                    if (!left) return Narrow(value.Sign < 0 ? -1 : 0, x, x);
                    if (value.IsZero) return Narrow(0, x, x);
                    throw new EvaluationException("Shift result exceeds supported integer representations.");
                }
                return Narrow(left ? value << (int)count : value >> (int)count, x, x);
            }
            // Java's unary promotion uses the left operand only; a long count does not widen it.
            int width = s.IntegerWidth ?? (x is long or ulong ? 64 : 32);
            if (s.ShiftCount == ShiftCountMode.ReverseDirection && count < 0) { left = !left; count = -count; }
            if (s.ShiftCount == ShiftCountMode.MaskToWidth) count &= width - 1;
            else if (count < 0 || count >= width) {
                if (s.ShiftCount == ShiftCountMode.ReverseDirection) return Narrow(0, x, x);
                throw new EvaluationException("Shift count is outside the supported integer width.");
            }
            bool unsigned = x is uint or ulong;
            if (!left && value.Sign < 0 && s.RightShift == RightShiftMode.ImplementationDefined)
                throw new EvaluationException("Right shifting a negative signed value requires target implementation semantics.");
            BigInteger mask = (BigInteger.One << width) - 1;
            if (!left && (s.RightShift == RightShiftMode.Logical || unsigned)) value &= mask;
            BigInteger shifted = left ? value << (int)count : value >> (int)count;
            if (s.ShiftCount == ShiftCountMode.RequireInRange && left && !unsigned
                && (value.Sign < 0 || shifted > (BigInteger.One << (width - 1)) - 1))
                throw new EvaluationException("Signed left shift requires defined target overflow semantics.");
            shifted &= mask;
            if (!unsigned && shifted >= (BigInteger.One << (width - 1))) shifted -= BigInteger.One << width;
            // Lua has a fixed 64-bit model, but keep small existing CLR literal representations where possible.
            if (s.ShiftCount == ShiftCountMode.ReverseDirection) return Narrow(shifted, x, x);
            if (width == 64) return unsigned ? (object)(ulong)shifted : (long)shifted;
            return unsigned ? (object)(uint)shifted : (int)shifted;
        }

        private static bool IsInteger(object? value) => value is sbyte or byte or short or ushort or char or int or uint or long or ulong;
        private static bool IsNumber(object value) => IsInteger(value) || value is float or double or decimal;
        private static BigInteger Integer(object value) => value switch {
            ulong u => new BigInteger(u), char c => new BigInteger(c), _ => new BigInteger(Convert.ToInt64(value)),
        };

        private static object Narrow(BigInteger value, object x, object y)
        {
            if (x is ulong || y is ulong) {
                if (value >= 0 && value <= ulong.MaxValue) return (ulong)value;
            } else if (x is long || y is long) {
                if (value >= long.MinValue && value <= long.MaxValue) return (long)value;
            } else if (x is uint || y is uint) {
                if (value >= 0 && value <= uint.MaxValue) return (uint)value;
            } else if (value >= int.MinValue && value <= int.MaxValue) return (int)value;
            // Widen only where the operation's result cannot fit the original representation.
            if (value >= long.MinValue && value <= long.MaxValue) return (long)value;
            throw new EvaluationException("Result exceeds supported integer representations.");
        }
    }
}
