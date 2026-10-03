// LINVAST - Language-INvariant AST library
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

using System.Text;

namespace LINVAST.Imperative.Builders.C
{
    /// <summary>
    /// Strips C preprocessor directives from source text so that the result can be
    /// lexed and parsed without a full preprocessor implementation.
    /// </summary>
    /// <remarks>
    /// Only the directive text (the lines that begin, after optional whitespace,
    /// with <c>#</c>) is removed. All other code is left untouched, which means:
    /// <list type="bullet">
    /// <item><description><c>#include</c>, <c>#define</c>, <c>#pragma</c>, <c>#line</c>,
    ///            <c>#error</c>, <c>#warning</c>, <c>#undef</c>, <c>#ifdef</c>/<c>#ifndef</c>/<c>#if</c>/<c>#elif</c>/<c>#else</c>/<c>#endif</c>,
    ///            <c>#include_next</c>, and the null directive are dropped.</description></item>
    /// <item><description>Macro names referenced by the source after a <c>#define</c> are
    ///            <em>not</em> expanded; they remain ordinary identifiers.</description></item>
    /// <item><description>Conditional bodies are <em>not</em> evaluated, so code under
    ///            <c>#if 0</c>/<c>#ifdef</c> is still handed to the parser.</description></item>
    /// </list>
    /// Directive characters are blanked (replaced with spaces), and newlines are
    /// preserved so that line numbers in the output match the original source,
    /// keeping AST line/column information accurate.
    /// </remarks>
    internal static class CPreprocessor
    {
        /// <summary>
        /// Returns a copy of <paramref name="source"/> with all preprocessing directives removed.
        /// </summary>
        /// <param name="source">The C source code, as supplied to the lexer.</param>
        /// <returns>Source code with directive lines blanked out.</returns>
        public static string StripDirectives(string source)
        {
            if (source.Length == 0)
                return source;

            var sb = new StringBuilder(source.Length);
            int i = 0;
            int n = source.Length;

            // Whether the current position is the start of a logical line (only
            // whitespace and comments have been seen since the last newline). A
            // '#' in this state begins a preprocessing directive.
            bool atLineStart = true;

            while (i < n) {
                char c = source[i];

                // --- Newlines -------------------------------------------------
                if (c == '\n') {
                    sb.Append(c);
                    atLineStart = true;
                    i++;
                    continue;
                }
                if (c == '\r') {
                    sb.Append(c);
                    if (i + 1 < n && source[i + 1] == '\n') {
                        sb.Append('\n');
                        i += 2;
                    } else {
                        i++;
                    }
                    atLineStart = true;
                    continue;
                }

                // --- Whitespace (does not reset atLineStart) -----------------
                if (c == ' ' || c == '\t') {
                    sb.Append(c);
                    i++;
                    continue;
                }

                // --- '#' at the start of a line begins a directive -----------
                if (c == '#' && atLineStart) {
                    i = ConsumeDirectiveLine(sb, source, i, n);
                    // The directive ends on a (possibly continued) line; the
                    // terminating newline was emitted inside ConsumeDirectiveLine
                    // and starts a fresh line for the following code.
                    atLineStart = true;
                    continue;
                }

                // --- Block comment /* ... */ --------------------------------
                if (c == '/' && i + 1 < n && source[i + 1] == '*') {
                    i = ConsumeBlockComment(sb, source, i, n, ref atLineStart, blank: false);
                    continue;
                }

                // --- Line comment // ... ------------------------------------
                if (c == '/' && i + 1 < n && source[i + 1] == '/') {
                    i = ConsumeLineComment(sb, source, i, n, blank: false);
                    continue;
                }

                // --- String literal "..." -----------------------------------
                if (c == '"') {
                    i = ConsumeStringOrChar(sb, source, i, n, '"', ref atLineStart, blank: false);
                    continue;
                }

                // --- Character constant '...' -------------------------------
                if (c == '\'') {
                    i = ConsumeStringOrChar(sb, source, i, n, '\'', ref atLineStart, blank: false);
                    continue;
                }

                // --- Any other significant character ------------------------
                atLineStart = false;
                sb.Append(c);
                i++;
            }

            return sb.ToString();
        }

        /// <summary>
        /// Consumes a preprocessing directive, blanking every character of the
        /// logical line (including backslash-newline continuations and any
        /// comments/strings embedded in the directive body) while preserving
        /// newlines so that line numbering is unchanged.
        /// </summary>
        /// <returns>The index just past the terminating newline (or end of input).</returns>
        private static int ConsumeDirectiveLine(StringBuilder sb, string source, int i, int n)
        {
            // '#', optional whitespace, and the directive body are all blanked.
            sb.Append(' ');   // '#'
            i++;

            bool atLineStart = false;
            while (i < n && (source[i] == ' ' || source[i] == '\t')) {
                sb.Append(' ');
                i++;
            }

            while (i < n) {
                char c = source[i];
                char next = i + 1 < n ? source[i + 1] : '\0';

                // Backslash-newline: line splicing inside the directive.
                if (c == '\\' && (next == '\n' || next == '\r')) {
                    sb.Append(' '); // blank the backslash
                    if (next == '\r') {
                        sb.Append('\r');
                        if (i + 2 < n && source[i + 2] == '\n') {
                            sb.Append('\n');
                            i += 3;
                        } else {
                            i += 2;
                        }
                    } else {
                        sb.Append('\n');
                        i += 2;
                    }
                    continue;
                }

                // Newline ends the directive (no preceding backslash).
                if (c == '\n') {
                    sb.Append(c);
                    return i + 1;
                }
                if (c == '\r') {
                    sb.Append('\r');
                    if (i + 1 < n && source[i + 1] == '\n') {
                        sb.Append('\n');
                        return i + 2;
                    }
                    return i + 1;
                }

                // Embedded comments/strings are part of the directive body and
                // are blanked wholesale (their content is replaced with spaces,
                // but newlines within are preserved).
                if (c == '/' && next == '*') {
                    i = ConsumeBlockComment(sb, source, i, n, ref atLineStart, blank: true);
                    continue;
                }
                if (c == '/' && next == '/') {
                    i = ConsumeLineComment(sb, source, i, n, blank: true);
                    continue;
                }
                if (c == '"') {
                    i = ConsumeStringOrChar(sb, source, i, n, '"', ref atLineStart, blank: true);
                    continue;
                }
                if (c == '\'') {
                    i = ConsumeStringOrChar(sb, source, i, n, '\'', ref atLineStart, blank: true);
                    continue;
                }

                sb.Append(' ');
                i++;
            }

            // Reached end of input inside a directive: nothing left to emit.
            return i;
        }

        /// <summary>
        /// Consumes a block comment.
        /// </summary>
        /// <param name="blank">
        /// When <c>true</c>, comment content is replaced with spaces (used inside
        /// directive bodies); newines are always preserved. When <c>false</c>,
        /// the comment text is copied verbatim.
        /// </param>
        /// <returns>The index just past the closing <c>*/</c>.</returns>
        private static int ConsumeBlockComment(StringBuilder sb, string source, int i, int n, ref bool atLineStart, bool blank)
        {
            // '/*' start (already at '/' on entry)
            if (blank) {
                sb.Append(' ');
                sb.Append(' ');
            } else {
                sb.Append(source[i]);      // '/'
                sb.Append(source[i + 1]);  // '*'
            }
            i += 2;

            bool commentCrossedLine = false;
            while (i < n) {
                char c = source[i];
                char next = i + 1 < n ? source[i + 1] : '\0';

                if (c == '*' && next == '/') {
                    if (blank) {
                        sb.Append(' ');
                        sb.Append(' ');
                    } else {
                        sb.Append('*');
                        sb.Append('/');
                    }
                    i += 2;
                    // A comment is whitespace: a '#' after it on the same line is
                    // still a directive when nothing significant preceded it.
                    // If the comment crossed a newline we are on a fresh line.
                    atLineStart = commentCrossedLine;
                    return i;
                }

                if (c == '\r') {
                    sb.Append('\r');
                    if (i + 1 < n && source[i + 1] == '\n') {
                        sb.Append('\n');
                        i += 2;
                    } else {
                        i++;
                    }
                    commentCrossedLine = true;
                    atLineStart = true;
                    continue;
                }
                if (c == '\n') {
                    sb.Append('\n');
                    i++;
                    commentCrossedLine = true;
                    atLineStart = true;
                    continue;
                }

                if (blank)
                    sb.Append(' ');
                else
                    sb.Append(c);
                i++;
            }

            // Unterminated comment: keep what we have; atLineStart stays as-is.
            return i;
        }

        /// <summary>
        /// Consumes a line comment, copying it verbatim (or blanking it when inside
        /// a directive body) while preserving the trailing newline.
        /// </summary>
        private static int ConsumeLineComment(StringBuilder sb, string source, int i, int n, bool blank)
        {
            if (blank) {
                sb.Append(' ');
                sb.Append(' ');
            } else {
                sb.Append(source[i]);      // '/'
                sb.Append(source[i + 1]);  // '/'
            }
            i += 2;

            while (i < n) {
                char c = source[i];

                if (c == '\r') {
                    sb.Append('\r');
                    if (i + 1 < n && source[i + 1] == '\n') {
                        sb.Append('\n');
                        return i + 2;
                    }
                    return i + 1;
                }
                if (c == '\n') {
                    sb.Append('\n');
                    return i + 1;
                }

                if (blank)
                    sb.Append(' ');
                else
                    sb.Append(c);
                i++;
            }

            return i;
        }

        /// <summary>
        /// Consumes a string literal or character constant, copying it verbatim
        /// (or blanking it when inside a directive body). Handles backslash
        /// escapes and backslash-newline line splicing.
        /// </summary>
        private static int ConsumeStringOrChar(StringBuilder sb, string source, int i, int n, char quote, ref bool atLineStart, bool blank)
        {
            if (blank)
                sb.Append(' ');
            else
                sb.Append(source[i]); // opening quote
            i++;

            while (i < n) {
                char c = source[i];

                // Escape: backslash followed by anything (including newline) is
                // consumed literally. '\' + newline is a line splice.
                if (c == '\\' && i + 1 < n) {
                    if (blank) {
                        sb.Append(' ');
                        sb.Append(' ');
                    } else {
                        sb.Append('\\');
                        sb.Append(source[i + 1]);
                    }
                    if (source[i + 1] == '\r' && i + 2 < n && source[i + 2] == '\n') {
                        sb.Append('\n');
                        i += 3;
                    } else {
                        i += 2;
                    }
                    continue;
                }

                if (c == quote) {
                    if (blank)
                        sb.Append(' ');
                    else
                        sb.Append(quote);
                    i++;
                    atLineStart = false; // the literal (or any part of it) is significant
                    return i;
                }

                if (c == '\r') {
                    sb.Append('\r');
                    if (i + 1 < n && source[i + 1] == '\n') {
                        sb.Append('\n');
                        i += 2;
                    } else {
                        i++;
                    }
                    continue;
                }
                if (c == '\n') {
                    sb.Append('\n');
                    i++;
                    continue;
                }

                if (blank)
                    sb.Append(' ');
                else
                    sb.Append(c);
                i++;
            }

            // Unterminated literal: keep what we have.
            atLineStart = false;
            return i;
        }
    }
}
