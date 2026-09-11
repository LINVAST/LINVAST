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
using System.Collections.Generic;
using System.Collections.Immutable;

namespace LINVAST.Imperative.Nodes.Common
{
    /// <summary>
    /// Provides mappings between language type names and .NET <see cref="TypeCode"/> values.
    /// </summary>
    public static class Types
    {
        private static ImmutableDictionary<string, TypeCode> _types = new Dictionary<string, TypeCode>() {
            // TODO add _t types
            { "byte" , TypeCode.Byte },
            { "signed byte" , TypeCode.SByte },
            { "bool" , TypeCode.Boolean },
            { "boolean" , TypeCode.Boolean },
            { "char" , TypeCode.Char },
            { "short" , TypeCode.Int16 },
            { "signed short" , TypeCode.Int16 },
            { "unsigned short" , TypeCode.UInt16 },
            { "integer" , TypeCode.Int32 },
            { "int" , TypeCode.Int32 },
            { "signed int" , TypeCode.Int32 },
            { "unsigned int" , TypeCode.UInt32 },
            { "long" , TypeCode.Int64 },
            { "long long" , TypeCode.Int64 },
            { "signed long" , TypeCode.Int64 },
            { "signed long long" , TypeCode.Int64 },
            { "unsigned long" , TypeCode.UInt64 },
            { "unsigned long long" , TypeCode.UInt64 },
            { "float" , TypeCode.Single },
            { "single" , TypeCode.Single },
            { "double" , TypeCode.Double },
            { "real" , TypeCode.Double },
            { "decimal" , TypeCode.Decimal },
            { "string" , TypeCode.String },
        }.ToImmutableDictionary();


        /// <summary>
        /// Gets the <see cref="TypeCode"/> for the specified type name.
        /// </summary>
        /// <param name="name">The type name to look up.</param>
        /// <returns>The corresponding <see cref="TypeCode"/>, or <c>null</c> if the type name is unknown.</returns>
        public static TypeCode? TypeCodeFor(string name)
            => _types.GetValueOrDefault(name.ToLower());

        /// <summary>
        /// Converts a <see cref="TypeCode"/> to its corresponding .NET <see cref="Type"/>.
        /// </summary>
        /// <param name="code">The type code to convert.</param>
        /// <returns>The corresponding <see cref="Type"/>, or <c>null</c> if the type code is not mapped.</returns>
        public static Type? ToType(this TypeCode code)
        {
            return code switch
            {
                TypeCode.Boolean => typeof(bool),
                TypeCode.Byte => typeof(byte),
                TypeCode.Char => typeof(char),
                TypeCode.DateTime => typeof(DateTime),
                TypeCode.DBNull => typeof(DBNull),
                TypeCode.Decimal => typeof(decimal),
                TypeCode.Double => typeof(double),
                TypeCode.Empty => null,
                TypeCode.Int16 => typeof(short),
                TypeCode.Int32 => typeof(int),
                TypeCode.Int64 => typeof(long),
                TypeCode.Object => typeof(object),
                TypeCode.SByte => typeof(sbyte),
                TypeCode.Single => typeof(float),
                TypeCode.String => typeof(string),
                TypeCode.UInt16 => typeof(ushort),
                TypeCode.UInt32 => typeof(uint),
                TypeCode.UInt64 => typeof(ulong),
                _ => null,
            };
        }
    }
}
