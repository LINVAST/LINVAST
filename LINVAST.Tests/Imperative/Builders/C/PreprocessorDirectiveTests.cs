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


using System.Linq;
using LINVAST.Exceptions;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Nodes;
using LINVAST.Nodes;
using LINVAST.Tests.Imperative.Builders.Common;
using NUnit.Framework;

namespace LINVAST.Tests.Imperative.Builders.C
{
    internal sealed class PreprocessorDirectiveTests : SourceComponentTestsBase
    {
        [Test]
        public void IncludeGuardsTest()
        {
            SourceNode sc = this.AssertTranslationUnit(
                "#ifndef FOO_H\n" +
                "#define FOO_H\n" +
                "int f(void) { return 0; }\n" +
                "#endif\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void DefineAndIncludeTest()
        {
            SourceNode sc = this.AssertTranslationUnit(
                "#include <stdio.h>\n" +
                "#define MAX(a,b) ((a)>(b)?(a):(b))\n" +
                "int main(void) { return MAX(1,2); }\n");
            FuncNode fn = sc.Children.Single().As<FuncNode>();
            Assert.That(fn.Identifier, Is.EqualTo("main"));
        }

        [Test]
        public void NullDirectiveTest()
        {
            SourceNode sc = this.AssertTranslationUnit(
                "#\n" +
                "void g(void) { }\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void ErrorAndWarningDirectivesTest()
        {
            SourceNode sc = this.AssertTranslationUnit(
                "#error \"not relevant\"\n" +
                "#warning \"just a warning\"\n" +
                "void h(void) { }\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void ConditionalBodyIsNotEvaluatedTest()
        {
            // #if / #else / #endif lines are stripped, but the lines *between*
            // them are ordinary source and are still parsed.
            SourceNode sc = this.AssertTranslationUnit(
                "int f(void) {\n" +
                "  int kept = 0;\n" +
                "#if 0\n" +
                "  int leftover = 1;\n" +
                "#else\n" +
                "  int chosen = 2;\n" +
                "#endif\n" +
                "  return kept;\n" +
                "}\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void LineContinuationInDefineTest()
        {
            SourceNode sc = this.AssertTranslationUnit(
                "#define SQUARE(x) \\\n" +
                "  ((x) * (x))\n" +
                "int sq(void) { return SQUARE(3); }\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void HashInsideStringAndCharTest()
        {
            // A '#' inside a string literal or character constant must never
            // start a directive.
            SourceNode sc = this.AssertTranslationUnit(
                "void lit(void) {\n" +
                "  char c = '#';\n" +
                "  const char* s = \"hash # inside\";\n" +
                "}\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void CommentContainingHashTest()
        {
            // A '#' inside a line comment must not be treated as a directive.
            SourceNode sc = this.AssertTranslationUnit(
                "// a # comment that must not strip the next line\n" +
                "void after(void) { }\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void DirectiveWithTrailingSpacesTest()
        {
            // Leading whitespace before '#', spaces after, then a newline.
            SourceNode sc = this.AssertTranslationUnit(
                "   #  define NOOP\n" +
                "void spaced(void) { }\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        [Test]
        public void LineNumbersArePreservedTest()
        {
            // The function lives on line 4 of the original source; stripping
            // directive lines must not shift it.
            SourceNode sc = this.AssertTranslationUnit(
                "#ifndef X\n" +
                "#define X\n" +
                "#endif\n" +
                "void nth(void) { }\n");
            FuncNode fn = sc.Children.Single().As<FuncNode>();
            Assert.That(fn.Line, Is.EqualTo(4));
        }

        [Test]
        public void WindowsLineEndingsTest()
        {
            SourceNode sc = this.AssertTranslationUnit(
                "#define WIN\r\n" +
                "int win(void) { return 1; }\r\n");
            Assert.That(sc.Children.Single(), Is.InstanceOf<FuncNode>());
        }

        protected override ASTNode GenerateAST(string src)
            => new CASTBuilder().BuildFromSource(src);
    }

    internal sealed class PreprocessorDirectiveErrors : BuildingErrorTestsBase
    {
        [Test]
        public void MidLineHashIsNotADirectiveTest()
        {
            // A '#' that is not at the start of a line is significant source
            // text and reaches the parser, which rejects it.
            this.AssertThrows<SyntaxErrorException>("int a = #b;\n");
        }

        protected override ASTNode GenerateAST(string src)
            => new CASTBuilder().BuildFromSource(src);
    }
}
