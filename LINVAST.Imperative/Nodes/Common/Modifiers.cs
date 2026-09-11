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


using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Linq;
using System.Text;

namespace LINVAST.Imperative.Nodes.Common
{
    /// <summary>
    /// Represents a set of declaration modifiers including access modifiers and qualifier flags.
    /// </summary>
    [DebuggerDisplay("{AccessModifiers} | {QualifierFlags}")]
    public sealed class Modifiers : IEquatable<Modifiers>
    {
        /// <summary>
        /// Parses a string of declaration modifiers into a <see cref="Modifiers"/> instance.
        /// </summary>
        /// <param name="specs">The modifier string to parse (e.g. "public static const").</param>
        /// <returns>A <see cref="Modifiers"/> instance representing the parsed modifiers.</returns>
        public static Modifiers Parse(string specs)
        {
            AccessModifiers access = AccessModifiers.Unspecified;

            string[] split = specs.ToLowerInvariant()
                .Split(" ", StringSplitOptions.RemoveEmptyEntries)
                .Distinct()
                .ToArray();

            if (split.Contains("private") || split.Contains("local"))
                access = AccessModifiers.Private;
            else if (split.Contains("protected"))
                access = AccessModifiers.Protected;
            else if (split.Contains("internal"))
                access = AccessModifiers.Internal;
            else if (split.Contains("public") || split.Contains("extern"))
                access = AccessModifiers.Public;

            QualifierFlags qualifiers = QualifierFlags.None;
            if (split.Contains("const") || split.Contains("final"))
                qualifiers |= QualifierFlags.Const;
            if (split.Contains("static"))
                qualifiers |= QualifierFlags.Static;
            if (split.Contains("volatile"))
                qualifiers |= QualifierFlags.Volatile;
            if (split.Contains("default"))
                qualifiers |= QualifierFlags.Default;

            return new Modifiers(access, qualifiers);
        }


        /// <summary>
        /// Gets the access modifier.
        /// </summary>
        public AccessModifiers AccessModifiers { get; }
        /// <summary>
        /// Gets the qualifier flags.
        /// </summary>
        public QualifierFlags QualifierFlags { get; }


        private Modifiers(AccessModifiers accessModifiers, QualifierFlags qualifiers)
        {
            this.AccessModifiers = accessModifiers;
            this.QualifierFlags = qualifiers;
        }


        /// <summary>
        /// Returns the string representation of the modifiers.
        /// </summary>
        /// <returns>A space-separated string of modifier names.</returns>
        public override string ToString()
        {
            var sb = new StringBuilder();
            switch (this.AccessModifiers) {
                case AccessModifiers.Private: sb.Append("private "); break;
                case AccessModifiers.Protected: sb.Append("protected "); break;
                case AccessModifiers.Internal: sb.Append("internal "); break;
                case AccessModifiers.Public: sb.Append("public "); break;
            }
            if (this.QualifierFlags.HasFlag(QualifierFlags.Static))
                sb.Append("static ");
            if (this.QualifierFlags.HasFlag(QualifierFlags.Const))
                sb.Append("const ");
            if (this.QualifierFlags.HasFlag(QualifierFlags.Volatile))
                sb.Append("volatile ");
            if (this.QualifierFlags.HasFlag(QualifierFlags.Default))
                sb.Append("default ");
            return sb.ToString().Trim();
        }

        /// <summary>
        /// Determines whether the specified object is equal to this <see cref="Modifiers"/> instance.
        /// </summary>
        /// <param name="obj">The object to compare.</param>
        /// <returns><c>true</c> if the object is equal; otherwise, <c>false</c>.</returns>
        public override bool Equals(object? obj)
            => this.Equals(obj as Modifiers);

        /// <summary>
        /// Determines whether the specified <see cref="Modifiers"/> is equal to this instance.
        /// </summary>
        /// <param name="other">The modifiers to compare.</param>
        /// <returns><c>true</c> if the modifiers are equal; otherwise, <c>false</c>.</returns>
        public bool Equals([AllowNull] Modifiers other)
        {
            if (other is null)
                return false;

            if (ReferenceEquals(this, other))
                return true;

            return this.AccessModifiers.Equals(other.AccessModifiers) && this.QualifierFlags.Equals(other.QualifierFlags);
        }

        /// <summary>
        /// Returns the hash code for this <see cref="Modifiers"/> instance.
        /// </summary>
        /// <returns>A hash code combining the access modifiers and qualifier flags.</returns>
        public override int GetHashCode() => (this.AccessModifiers, this.QualifierFlags).GetHashCode();
    }

    /// <summary>
    /// Defines the access modifiers for a declaration.
    /// </summary>
    public enum AccessModifiers
    {
        /// <summary>
        /// No access modifier specified.
        /// </summary>
        Unspecified = 0,
        /// <summary>
        /// Private access.
        /// </summary>
        Private,
        /// <summary>
        /// Protected access.
        /// </summary>
        Protected,
        /// <summary>
        /// Internal access.
        /// </summary>
        Internal,
        /// <summary>
        /// Public access.
        /// </summary>
        Public
    }

    /// <summary>
    /// Defines qualifier flags that can be applied to a declaration.
    /// </summary>
    [Flags]
    public enum QualifierFlags
    {
        /// <summary>
        /// No qualifiers.
        /// </summary>
        None = 0,
        /// <summary>
        /// Static qualifier.
        /// </summary>
        Static = 1,
        /// <summary>
        /// Const qualifier.
        /// </summary>
        Const = 2,
        /// <summary>
        /// Volatile qualifier.
        /// </summary>
        Volatile = 4,
        /// <summary>
        /// Default qualifier.
        /// </summary>
        Default = 8,
    }
}
