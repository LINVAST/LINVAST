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

namespace LINVAST.Imperative.Nodes.Common
{
    /// <summary>
    /// Defines the type of a jump statement (return, continue, break, or goto).
    /// </summary>
    public enum JumpStatType
    {
        /// <summary>
        /// A return statement.
        /// </summary>
        Return,
        /// <summary>
        /// A continue statement.
        /// </summary>
        Continue,
        /// <summary>
        /// A break statement.
        /// </summary>
        Break,
        /// <summary>
        /// A goto statement.
        /// </summary>
        Goto
    }

    /// <summary>
    /// Provides conversion methods for jump statement types and their string representations.
    /// </summary>
    public static class JumpStatementTypeConverter
    {
        /// <summary>
        /// Converts a string token to the corresponding <see cref="JumpStatType"/>.
        /// </summary>
        /// <param name="str">The string token to convert.</param>
        /// <returns>The matching <see cref="JumpStatType"/>.</returns>
        /// <exception cref="ArgumentException">Thrown when the string does not match a valid jump statement token.</exception>
        public static JumpStatType FromString(string str)
        {
            return str switch
            {
                "return" => JumpStatType.Return,
                "continue" => JumpStatType.Continue,
                "break" => JumpStatType.Break,
                "goto" => JumpStatType.Goto,
                _ => throw new ArgumentException("Invalid jump statement token"),
            };
        }

        /// <summary>
        /// Converts a <see cref="JumpStatType"/> to its string token representation.
        /// </summary>
        /// <param name="type">The jump statement type to convert.</param>
        /// <returns>The string token representing the jump statement type.</returns>
        /// <exception cref="ArgumentException">Thrown when the jump statement type is invalid.</exception>
        public static string ToStringToken(this JumpStatType type)
        {
            return type switch
            {
                JumpStatType.Break => "break",
                JumpStatType.Continue => "continue",
                JumpStatType.Goto => "goto",
                JumpStatType.Return => "return",
                _ => throw new ArgumentException("Invalid jump statement value"),
            };
        }
    }
}
