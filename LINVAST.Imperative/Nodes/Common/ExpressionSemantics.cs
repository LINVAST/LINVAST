using System;
using Newtonsoft.Json;

namespace LINVAST.Imperative.Nodes.Common
{
    public enum DivisionMode { True, Floor, TruncateIntegers }
    public enum RemainderMode { DividendSign, DivisorSign }
    public enum TruthinessMode { BooleanOnly, NumericNonzero, Python, Lua }
    public enum LogicalResultMode { Boolean, SelectedOperand }
    public enum RightShiftMode { Arithmetic, Logical, ImplementationDefined }
    public enum ShiftCountMode { MaskToWidth, Unbounded, ReverseDirection, RequireInRange }
    public enum ZeroDivisionMode { Throw, Ieee754 }

    /// <summary>
    /// Immutable behavior metadata for one expression, independent of its spelling and source language.
    /// Unspecified fields retain legacy behavior. This value is not an AST child.
    /// </summary>
    public sealed record ExpressionSemantics
    {
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public DivisionMode? Division { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public RemainderMode? Remainder { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public TruthinessMode? Truthiness { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public LogicalResultMode? LogicalResult { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public RightShiftMode? RightShift { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public ShiftCountMode? ShiftCount { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public int? IntegerWidth { get; }
        [JsonProperty(NullValueHandling = NullValueHandling.Ignore)]
        public ZeroDivisionMode? ZeroDivision { get; }

        public ExpressionSemantics(DivisionMode? division = null, RemainderMode? remainder = null,
            TruthinessMode? truthiness = null, LogicalResultMode? logicalResult = null,
            RightShiftMode? rightShift = null, ShiftCountMode? shiftCount = null,
            int? integerWidth = null, ZeroDivisionMode? zeroDivision = null)
        {
            ValidateEnum(division); ValidateEnum(remainder); ValidateEnum(truthiness);
            ValidateEnum(logicalResult); ValidateEnum(rightShift); ValidateEnum(shiftCount); ValidateEnum(zeroDivision);
            if (integerWidth is not null and not 32 and not 64)
                throw new ArgumentOutOfRangeException(nameof(integerWidth), "Integer width must be 32 or 64.");
            if (logicalResult is not null && truthiness is null)
                throw new ArgumentException("Logical result selection requires a truthiness rule.");
            if ((rightShift is null) != (shiftCount is null))
                throw new ArgumentException("Shift behavior requires both direction and count rules.");
            if (integerWidth is not null && shiftCount is null)
                throw new ArgumentException("Integer width requires shift semantics.");
            if (shiftCount == ShiftCountMode.Unbounded && (integerWidth is not null || rightShift != RightShiftMode.Arithmetic))
                throw new ArgumentException("Unbounded shifts require arithmetic right shift and no fixed width.");
            if (zeroDivision is not null && division is null && remainder is null)
                throw new ArgumentException("Zero-division behavior requires division or remainder semantics.");
            int groups = (division is not null ? 1 : 0) + (remainder is not null ? 1 : 0)
                + (truthiness is not null ? 1 : 0) + (rightShift is not null ? 1 : 0);
            if (groups != 1)
                throw new ArgumentException("Specify exactly one operation's semantics.");
            this.Division = division;
            this.Remainder = remainder;
            this.Truthiness = truthiness;
            this.LogicalResult = logicalResult;
            this.RightShift = rightShift;
            this.ShiftCount = shiftCount;
            this.IntegerWidth = integerWidth;
            this.ZeroDivision = zeroDivision;
        }

        private static void ValidateEnum<T>(T? value) where T : struct, Enum
        {
            if (value.HasValue && !Enum.IsDefined(value.Value))
                throw new ArgumentOutOfRangeException(nameof(value), "Unknown semantics value.");
        }
    }
}
