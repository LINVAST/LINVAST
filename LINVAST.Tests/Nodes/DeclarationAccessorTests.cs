using System;
using System.Linq;
using LINVAST.Imperative.Nodes;
using NUnit.Framework;

namespace LINVAST.Tests.Nodes
{
    internal sealed class DeclarationAccessorTests
    {
        [TestCase(0, false)]
        [TestCase(1, false)]
        [TestCase(2, false)]
        [TestCase(0, true)]
        [TestCase(1, true)]
        [TestCase(2, true)]
        public void VariableAccessorsSkipTags(int tagCount, bool initialized)
        {
            var tags = Enumerable.Range(0, tagCount).Select(i => new TagNode(1, "Tag" + i)).ToArray();
            var id = new IdNode(1, "x");
            var value = new LitExprNode(1, 42);
            var node = initialized ? new VarDeclNode(1, tags, id, value) : new VarDeclNode(1, tags, id);
            Assert.That(node.IdentifierNode, Is.SameAs(id));
            Assert.That(node.Identifier, Is.EqualTo("x"));
            Assert.That(node.Initializer, initialized ? Is.SameAs(value) : Is.Null);
            Assert.That(node.Tags.Count(), Is.EqualTo(tagCount));
            Assert.That(node.GetText(), Is.EqualTo(initialized ? "x = 42" : "x"));
        }

        [TestCase(false)]
        [TestCase(true)]
        public void FunctionAccessorsSkipTags(bool withBody)
        {
            var tags = new[] { new TagNode(1, "A"), new TagNode(1, "B") };
            var id = new IdNode(1, "f");
            var templates = new TypeNameListNode(1, new TypeNameNode(1, "T"));
            var parameters = new FuncParamsNode(1, new FuncParamNode(1,
                new DeclSpecsNode(1, "int"), new VarDeclNode(1, new IdNode(1, "x"))));
            var body = new BlockStatNode(1);
            var node = withBody
                ? new FuncDeclNode(1, tags, id, templates, parameters, body)
                : new FuncDeclNode(1, tags, id, templates, parameters);
            Assert.That(node.IdentifierNode, Is.SameAs(id));
            Assert.That(node.TemplateArgs, Is.SameAs(templates));
            Assert.That(node.ParametersNode, Is.SameAs(parameters));
            Assert.That(node.Definition, withBody ? Is.SameAs(body) : Is.Null);
            Assert.That(node.Parameters!.Single().Declarator.Identifier, Is.EqualTo("x"));
        }

        [TestCase(false)]
        [TestCase(true)]
        public void MinimalFunctionHasEmptyTemplateArguments(bool tagged)
        {
            var node = tagged ? new FuncDeclNode(1, new[] { new TagNode(1, "A") }, new IdNode(1, "f"))
                : new FuncDeclNode(1, new IdNode(1, "f"));
            Assert.That(node.TemplateArgs.Types, Is.Empty);
            Assert.That(node.TemplateArgs.Parent, Is.SameAs(node));
            Assert.That(node.ParametersNode, Is.Null);
            Assert.That(node.Definition, Is.Null);
            Assert.DoesNotThrow(() => node.GetText());
        }

        [Test]
        public void EnumBodyExcludesConstants()
        {
            var constants = new DeclListNode(1, new VarDeclNode(1, new IdNode(1, "A")));
            var member = new DeclStatNode(1, new DeclSpecsNode(1, "int"),
                new DeclListNode(1, new VarDeclNode(1, new IdNode(1, "value"))));
            var node = new EnumDeclNode(1, new IdNode(1, "E"), constants, new[] { member });
            Assert.That(node.Constants, Is.SameAs(constants));
            Assert.That(node.BodyDeclarations.Single(), Is.SameAs(member));
        }

        [Test]
        public void EmptyEnumConstructorsHaveConsistentAccessors()
        {
            var minimal = new EnumDeclNode(1, new IdNode(1, "E"));
            var explicitConstants = new EnumDeclNode(1, new IdNode(1, "E"), new DeclListNode(1));
            foreach (var node in new[] { minimal, explicitConstants }) {
                Assert.That(node.Constants.Declarators, Is.Empty);
                Assert.That(node.Constants.Parent, Is.SameAs(node));
                Assert.That(node.BodyDeclarations, Is.Empty);
            }
        }
    }
}
