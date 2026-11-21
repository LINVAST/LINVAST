# LINVAST - Language-INVariant AST library
[![Issues](https://img.shields.io/github/issues/LINVAST/LINVAST.svg)](https://github.com/LINVAST/LINVAST/issues)
[![Stable release](https://img.shields.io/github/release/LINVAST/LINVAST.svg?label=stable)](https://github.com/LINVAST/LINVAST/releases)
[![Latest release](https://img.shields.io/github/tag-pre/LINVAST/LINVAST.svg?label=latest)](https://github.com/LINVAST/LINVAST/releases)
[![NuGet](https://img.shields.io/nuget/vpre/LINVAST.svg?label=NuGet:%20LINVAST)](https://nuget.org/packages/LINVAST)
[![NuGet](https://img.shields.io/nuget/vpre/LINVAST.Imperative.svg?label=NuGet:%20LINVAST.Imperative)](https://nuget.org/packages/LINVAST.Imperative)
[![Stable release](https://img.shields.io/github/release/LINVAST/CLI.svg?label=linvast-cli)](https://github.com/LINVAST/CLI/releases)

LINVAST is a set of libraries that provide a common language-invariant AST API for different programming languages by abstracting [ANTLR](https://www.antlr.org/) parse trees. Currently, the main focus of the project is the imperative programming paradigm, with supported languages (in alphabetical order):

| Language | Grammar File(s) | Version Reference | Grammar Source |
|---|---|---|---|
| C | [`C.g4`](https://github.com/LINVAST/LINVAST/search?q=C.g4&type=code) | C11 | C 2011 grammar based on the C11 specification |
| Go | [`GoParser.g4`](https://github.com/LINVAST/LINVAST/search?q=GoParser.g4&type=code) / [`GoLexer.g4`](https://github.com/LINVAST/LINVAST/search?q=GoLexer.g4&type=code) | Go 1.25 | Derived from the Go language specification |
| Java | [`Java.g4`](https://github.com/LINVAST/LINVAST/search?q=Java.g4&type=code) / [`JavaLexer.g4`](https://github.com/LINVAST/LINVAST/search?q=JavaLexer.g4&type=code) | Java 8 | BSD-licensed ANTLR Java 8 grammar by Ivan Kochurkin |
| Kotlin | [`KotlinParser.g4`](https://github.com/LINVAST/LINVAST/search?q=KotlinParser.g4&type=code) | Kotlin 1.x | Based on Kotlin compiler test data |
| Lua | [`Lua.g4`](https://github.com/LINVAST/LINVAST/search?q=Lua.g4&type=code) | Lua 5.1–5.3 | Multi-version grammar tested against Lua 5.2/5.3 test suites |
| Python | [`Python3Parser.g4`](https://github.com/LINVAST/LINVAST/search?q=Python3Parser.g4&type=code) / [`Python3Lexer.g4`](https://github.com/LINVAST/LINVAST/search?q=Python3Lexer.g4&type=code) | Python 3 | ANTLR4 Python 3 grammar by Bart Kiers |

The Kotlin builder normalizes built-in type names: for example, `Int` becomes `int`, `Boolean` becomes `bool`, `String` becomes `string`, and `Unit` becomes `void`. Both `Array<Int>` and `IntArray` become `int[]`; nested arrays and nullable types retain their shape. Generic arguments are preserved in `TypeNameNode.TemplateArguments`, so `List<Array<Int>>` has the name `List` and an `int[]` argument. These rules also apply to return types, parameters, properties, and type aliases.

Other builders also normalize equivalent type spellings:

| Language | Source type | AST type name |
|---|---|---|
| Go | `[]int`, `[3]int`, `[2][3]int` | `int[]`, `int[3]`, `int[2][3]` |
| Go | `*int`, `[]*int`, `*[]int` | `int*`, `int*[]`, `int[]*` |
| Go | `float32`, `float64`, `int32`, `int64` | `float`, `double`, `int`, `long` |
| Go | `uint8`, `uint16`, `uint32`, `uint64` | `byte`, `unsigned short`, `unsigned int`, `unsigned long` |
| Go | `int8`, `int16`, `rune`, `any`, `interface{}` | `signed byte`, `short`, `int`, `object`, `object` |
| Java | `boolean`, `String`, `Object` | `bool`, `string`, `object` |
| Python annotations | `str`, `Any`, return annotation `None` | `string`, `object`, `void` |
| C | `_Bool` | `bool` |

Go normalization also covers composite literals, allocation arguments, and types nested in maps, channels, structs, interfaces, and function signatures. Array lengths and dimension order are retained. Go inferred literal types use canonical names such as `long` and `string` instead of .NET enum names such as `Int64` and `String`. Python collection annotations retain their collection syntax, and `None` inside a union remains `None`. Java boxed types and custom qualified names retain their names; generic arguments are preserved, including in qualified nested types and arrays such as `List<string>[]`.


## AST traversal and transformations

Loop components are part of `Children`, so generic traversal, equality, copying, and substitution include them. `ForStatNode` keeps its condition and body at indices 0 and 1, followed by its initializer (when present) and increment (when present). `HasInitializer` and `HasIncrement` distinguish these optional positions. In serialized JSON, the components appear once under `Children`; the two flags replace the need for separate `ForDeclaration`, `InitExpr`, and `IncrExpr` fields. Those accessors remain available in the C# API.

`Copy()` returns an independent tree with a detached root and parent links pointing into the copy. `Substitute()` matches structurally equal nodes, clones each replacement occurrence independently, and leaves both input trees unchanged. Replacement contents are copied without applying the substitution recursively to them. `BaseASTVisitor` supports repeated traversals without a cumulative node limit and visits the children of node types without a dedicated overload.

Constructors defensively copy child collections and expose them as read-only. They attach the supplied nodes by setting their parent links; use `Copy()` when attaching a subtree that must remain in another tree. Nodes still have mutable metadata and operator delegates, so the AST is not fully immutable. Copying duplicates AST nodes, but opaque objects stored as literal values and objects captured by delegates remain shared.

`TagFieldNode.Value` is now its first child. Traversal, copying, and substitution include tag values, and JSON serializes them once under `Children`, without a separate `Value` property on the tag field.

Equality and hashing use node types, children, and structural metadata, including pointer levels, import paths/aliases, package names, and try-clause roles. They ignore source locations, source filenames, literal suffix spellings after conversion, and opaque evaluation delegates. Hashing does not render the tree or depend on the current numeric display culture. Avoid changing a node's equality-relevant metadata while it is stored in a hash collection.

`SimplifyComplexAssignment()` returns an independent tree. It expands compound assignments to plain identifiers while retaining the original operator's evaluation delegate, including bitwise operations. Complex targets, such as array elements and dereferences, and specialized increment/decrement nodes retain their compound form so their evaluation order is preserved.

Symbolic wildcard names are derived from expression content using SHA-256 rather than a shared counter or cache. Separate and concurrent conversions produce the same wildcard for the same unsupported symbolic expression without retaining expression history. The `v__` identifier prefix is reserved for generated wildcards. `WildcardReplace()` recognizes complete generated names and legacy numeric names. Concurrent parsing is supported when the input tree and the builder's `Node` property are not being mutated.

Declaration accessors skip leading tags. Function declarations constructed with only an identifier now include an empty `TypeNameListNode` child, and enum declarations constructed with only an identifier include an empty `DeclListNode` for constants. These children also appear in JSON, making the corresponding accessors consistent with the other constructors.

Symbolic function calls have fresh opaque results on every visit, including calls with identical arguments. Their names include a fingerprint of the complete call and a unique occurrence identifier. No purity guarantee is assumed, so `f(1) - f(1)` does not simplify to zero. Results containing calls are intentionally not deterministic across conversions; the `r__` prefix is reserved for them. This representation prevents invalid cancellation but does not execute calls or model their side effects.

Python generated temporary names reset for each build and avoid names found throughout that source. Chained assignments capture their RHS once before assigning targets in source order. Kotlin preserves all prefix operators, parameter defaults, and expression-body returns; an inferred return type is represented as `var`.

Java compilation units retain their `PackageNode`. Explicit casts use `CastExprNode` with `TargetType` and `Operand` children. Both constant and symbolic evaluators throw `EvaluationException` for casts because source-language conversion rules are not yet implemented.

## Development scripts

The Linux build entrypoints require Bash and the .NET 10 SDK.

The release script also uses GNU `realpath` to validate its output directory. It preserves existing files and older packages in that directory; packages with the same output filename can be replaced by `dotnet pack`. The repository directory and its ancestors cannot be used as output directories, including through symlink aliases.

```bash
./build
./test
./release
```

Use `--configuration Debug|Release` on any script. `release` writes NuGet packages to `artifacts/` by default and accepts the same prerelease version inputs as the old rebuild scripts:

```bash
./release --version-suffix dev --build-number 42
./release --artifacts ./artifacts --configuration Release
```

## Motivation and project description
> *There are many programming languages out there and, even though their syntax might be different, they often derive from or use certain universal programming concepts. We also call that a _way of writing code_ or, more commonly, a programming paradigm. The motivation for LINVAST came from the inability to find a shared API for every programming language that is a part of a procedural paradigm. LINVAST aims to create a common abstraction for imperative (but also procedural, OO, script and, through a few concepts, functional) programming paradigm so that it is possible to view many different programming languages on the same level of abstraction.*
>
> *LINVAST can theoretically work with any programming language as long as the adapter for that language is written (hence, _Language Invariant_). Adapters (or, in further text, _Builders_) serve as an intermediary between ANTLR parse trees and language-invariant ASTs. Builders are used to generate AST from a parse tree and they are implemented differently for every programming language due to native differences in parse trees. LINVAST provides intuitive API for generating and traversing generated ASTs and also provides already implemented visitors for common AST operations such as expression evaluation.*
>
> *LINVAST was made as a proof of concept for my MSc thesis (_Semantic comparison of structurally similar imperative code segments_) but has grown with aim to become fully operational and manageable long-term. LINVAST can be used as a tool to generate serialized AST in JSON, and provides intuitive API which is used in many standalone programs which use LINVAST API to visualize or compare generated ASTs.*

## Examples

### Using LINVAST in your own codebase

```cs
using System;
using System.Linq;
using LINVAST.Nodes;
using LINVAST.Imperative.Nodes;
using LINVAST.Imperative.Builders.C;
using LINVAST.Imperative.Visitors;

var builder = new CASTBuilder();
ASTNode ast = builder.BuildFromSource("int value = 1 + 2;");
ASTNode copy = ast.Copy();
Console.WriteLine(ast == copy); // True

var declaration = ast.Children.Single().As<DeclStatNode>();
BinaryExprNode expr = declaration.DeclaratorList.Declarators.Single()
    .As<VarDeclNode>().Initializer!.As<BinaryExprNode>();

// Substitution returns an independent tree; ast still contains 1 + 2.
ASTNode changed = ast.Substitute(expr.RightOperand, new LitExprNode(expr.Line, 10));
Console.WriteLine(changed.GetText());
Console.WriteLine(ast.ToJson(compact: false));

if (ConstantExpressionEvaluator.TryEvaluateAs<int>(expr, out int result))
    Console.WriteLine(result); // 3

var assignment = builder.BuildFromSource("value += 1", p => p.expression())
    .As<AssignExprNode>();
ExprNode simplified = assignment.SimplifyComplexAssignment();
Console.WriteLine(simplified.GetText());
Console.WriteLine(new LiteralCounter().Visit(ast)); // 2

class LiteralCounter : BaseASTVisitor<int>
{
    public override int Visit(LitExprNode node) => 1;
    protected override int AggregateResult(int aggregate, int nextResult) => aggregate + nextResult;
}
```

### Language-invariant AST examples

Concrete examples on how to use LINVAST as a standalone CLI tool can be seen [here](https://github.com/LINVAST/CLI/).

#### AST generated from C source:
```c
int gl_y = 2;
void f(int x);
int gl_x = 3;

int main()
{
	int x = 1;
	//printf("Hello world!% d\n", x);
	return 0;
}

static int gl_z;
```

<details>
<summary>Click to expand!</summary>

```json
{
  "Name": null,
  "NodeType": "SourceNode",
  "Line": 1,
  "Children": [
    {
      "NodeType": "DeclStatNode",
      "Line": 1,
      "Children": [
        {
          "Modifiers": {
            "AccessModifiers": "Unspecified",
            "QualifierFlags": "None"
          },
          "NodeType": "DeclSpecsNode",
          "Line": 1,
          "Children": [
            {
              "TypeName": "int",
              "PointerLevel": 0,
              "NodeType": "TypeNameNode",
              "Line": 1,
              "Children": [
                {
                  "Identifier": "int",
                  "NodeType": "IdNode",
                  "Line": 1,
                  "Children": []
                }
              ]
            }
          ]
        },
        {
          "NodeType": "DeclListNode",
          "Line": 1,
          "Children": [
            {
              "PointerLevel": 0,
              "NodeType": "VarDeclNode",
              "Line": 1,
              "Children": [
                {
                  "Identifier": "gl_y",
                  "NodeType": "IdNode",
                  "Line": 1,
                  "Children": []
                },
                {
                  "Value": 2,
                  "Suffix": null,
                  "TypeCode": "Int32",
                  "NodeType": "LitExprNode",
                  "Line": 1,
                  "Children": []
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "NodeType": "DeclStatNode",
      "Line": 2,
      "Children": [
        {
          "Modifiers": {
            "AccessModifiers": "Unspecified",
            "QualifierFlags": "None"
          },
          "NodeType": "DeclSpecsNode",
          "Line": 2,
          "Children": [
            {
              "TypeName": "void",
              "PointerLevel": 0,
              "NodeType": "TypeNameNode",
              "Line": 2,
              "Children": [
                {
                  "Identifier": "void",
                  "NodeType": "IdNode",
                  "Line": 2,
                  "Children": []
                }
              ]
            }
          ]
        },
        {
          "NodeType": "DeclListNode",
          "Line": 2,
          "Children": [
            {
              "PointerLevel": 0,
              "NodeType": "FuncDeclNode",
              "Line": 2,
              "Children": [
                {
                  "Identifier": "f",
                  "NodeType": "IdNode",
                  "Line": 2,
                  "Children": []
                },
                {
                  "NodeType": "TypeNameListNode",
                  "Line": 2,
                  "Children": []
                },
                {
                  "IsVariadic": false,
                  "NodeType": "FuncParamsNode",
                  "Line": 2,
                  "Children": [
                    {
                      "NodeType": "FuncParamNode",
                      "Line": 2,
                      "Children": [
                        {
                          "Modifiers": {
                            "AccessModifiers": "Unspecified",
                            "QualifierFlags": "None"
                          },
                          "NodeType": "DeclSpecsNode",
                          "Line": 2,
                          "Children": [
                            {
                              "TypeName": "int",
                              "PointerLevel": 0,
                              "NodeType": "TypeNameNode",
                              "Line": 2,
                              "Children": [
                                {
                                  "Identifier": "int",
                                  "NodeType": "IdNode",
                                  "Line": 2,
                                  "Children": []
                                }
                              ]
                            }
                          ]
                        },
                        {
                          "PointerLevel": 0,
                          "NodeType": "VarDeclNode",
                          "Line": 2,
                          "Children": [
                            {
                              "Identifier": "x",
                              "NodeType": "IdNode",
                              "Line": 2,
                              "Children": []
                            }
                          ]
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "NodeType": "DeclStatNode",
      "Line": 3,
      "Children": [
        {
          "Modifiers": {
            "AccessModifiers": "Unspecified",
            "QualifierFlags": "None"
          },
          "NodeType": "DeclSpecsNode",
          "Line": 3,
          "Children": [
            {
              "TypeName": "int",
              "PointerLevel": 0,
              "NodeType": "TypeNameNode",
              "Line": 3,
              "Children": [
                {
                  "Identifier": "int",
                  "NodeType": "IdNode",
                  "Line": 3,
                  "Children": []
                }
              ]
            }
          ]
        },
        {
          "NodeType": "DeclListNode",
          "Line": 3,
          "Children": [
            {
              "PointerLevel": 0,
              "NodeType": "VarDeclNode",
              "Line": 3,
              "Children": [
                {
                  "Identifier": "gl_x",
                  "NodeType": "IdNode",
                  "Line": 3,
                  "Children": []
                },
                {
                  "Value": 3,
                  "Suffix": null,
                  "TypeCode": "Int32",
                  "NodeType": "LitExprNode",
                  "Line": 3,
                  "Children": []
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "NodeType": "FuncNode",
      "Line": 5,
      "Children": [
        {
          "Modifiers": {
            "AccessModifiers": "Unspecified",
            "QualifierFlags": "None"
          },
          "NodeType": "DeclSpecsNode",
          "Line": 5,
          "Children": [
            {
              "TypeName": "int",
              "PointerLevel": 0,
              "NodeType": "TypeNameNode",
              "Line": 5,
              "Children": [
                {
                  "Identifier": "int",
                  "NodeType": "IdNode",
                  "Line": 5,
                  "Children": []
                }
              ]
            }
          ]
        },
        {
          "NodeType": "DeclListNode",
          "Line": 5,
          "Children": [
            {
              "PointerLevel": 0,
              "NodeType": "FuncDeclNode",
              "Line": 5,
              "Children": [
                {
                  "Identifier": "main",
                  "NodeType": "IdNode",
                  "Line": 5,
                  "Children": []
                },
                {
                  "NodeType": "TypeNameListNode",
                  "Line": 5,
                  "Children": []
                },
                {
                  "NodeType": "BlockStatNode",
                  "Line": 7,
                  "Children": [
                    {
                      "NodeType": "DeclStatNode",
                      "Line": 7,
                      "Children": [
                        {
                          "Modifiers": {
                            "AccessModifiers": "Unspecified",
                            "QualifierFlags": "None"
                          },
                          "NodeType": "DeclSpecsNode",
                          "Line": 7,
                          "Children": [
                            {
                              "TypeName": "int",
                              "PointerLevel": 0,
                              "NodeType": "TypeNameNode",
                              "Line": 7,
                              "Children": [
                                {
                                  "Identifier": "int",
                                  "NodeType": "IdNode",
                                  "Line": 7,
                                  "Children": []
                                }
                              ]
                            }
                          ]
                        },
                        {
                          "NodeType": "DeclListNode",
                          "Line": 7,
                          "Children": [
                            {
                              "PointerLevel": 0,
                              "NodeType": "VarDeclNode",
                              "Line": 7,
                              "Children": [
                                {
                                  "Identifier": "x",
                                  "NodeType": "IdNode",
                                  "Line": 7,
                                  "Children": []
                                },
                                {
                                  "Value": 1,
                                  "Suffix": null,
                                  "TypeCode": "Int32",
                                  "NodeType": "LitExprNode",
                                  "Line": 7,
                                  "Children": []
                                }
                              ]
                            }
                          ]
                        }
                      ]
                    },
                    {
                      "Type": "Return",
                      "NodeType": "JumpStatNode",
                      "Line": 9,
                      "Children": [
                        {
                          "Value": 0,
                          "Suffix": null,
                          "TypeCode": "Int32",
                          "NodeType": "LitExprNode",
                          "Line": 9,
                          "Children": []
                        }
                      ]
                    }
                  ]
                }
              ]
            }
          ]
        }
      ]
    },
    {
      "NodeType": "DeclStatNode",
      "Line": 12,
      "Children": [
        {
          "Modifiers": {
            "AccessModifiers": "Unspecified",
            "QualifierFlags": "Static"
          },
          "NodeType": "DeclSpecsNode",
          "Line": 12,
          "Children": [
            {
              "TypeName": "int",
              "PointerLevel": 0,
              "NodeType": "TypeNameNode",
              "Line": 12,
              "Children": [
                {
                  "Identifier": "int",
                  "NodeType": "IdNode",
                  "Line": 12,
                  "Children": []
                }
              ]
            }
          ]
        },
        {
          "NodeType": "DeclListNode",
          "Line": 12,
          "Children": [
            {
              "PointerLevel": 0,
              "NodeType": "VarDeclNode",
              "Line": 12,
              "Children": [
                {
                  "Identifier": "gl_z",
                  "NodeType": "IdNode",
                  "Line": 12,
                  "Children": []
                }
              ]
            }
          ]
        }
      ]
    }
  ]
}
```
</details>


#### Compact AST generated from Lua source:
```lua
function fact (n)
  if n == 0 then
    return 1
  else
    return n * fact(n-1)
  end
end
```

<details>
<summary>Click to expand!</summary>

```json
{"Name":null,"NodeType":"SourceNode","Line":1,"Children":[{"NodeType":"FuncNode","Line":1,"Children":[{"Modifiers":{"AccessModifiers":"Unspecified","QualifierFlags":"None"},"NodeType":"DeclSpecsNode","Line":1,"Children":[{"TypeName":"object","PointerLevel":0,"NodeType":"TypeNameNode","Line":1,"Children":[{"Identifier":"object","NodeType":"IdNode","Line":1,"Children":[]}]}]},{"NodeType":"DeclListNode","Line":1,"Children":[{"PointerLevel":0,"NodeType":"FuncDeclNode","Line":1,"Children":[{"Identifier":"fact","NodeType":"IdNode","Line":1,"Children":[]},{"NodeType":"TypeNameListNode","Line":1,"Children":[]},{"IsVariadic":false,"NodeType":"FuncParamsNode","Line":1,"Children":[{"NodeType":"FuncParamNode","Line":1,"Children":[{"Modifiers":{"AccessModifiers":"Unspecified","QualifierFlags":"None"},"NodeType":"DeclSpecsNode","Line":1,"Children":[{"TypeName":"object","PointerLevel":0,"NodeType":"TypeNameNode","Line":1,"Children":[{"Identifier":"object","NodeType":"IdNode","Line":1,"Children":[]}]}]},{"PointerLevel":0,"NodeType":"VarDeclNode","Line":1,"Children":[{"Identifier":"n","NodeType":"IdNode","Line":1,"Children":[]}]}]}]},{"NodeType":"BlockStatNode","Line":2,"Children":[{"NodeType":"IfStatNode","Line":2,"Children":[{"NodeType":"RelExprNode","Line":2,"Children":[{"Identifier":"n","NodeType":"IdNode","Line":2,"Children":[]},{"Symbol":"==","NodeType":"RelOpNode","Line":2,"Children":[]},{"Value":0,"Suffix":null,"TypeCode":"Int32","NodeType":"LitExprNode","Line":2,"Children":[]}]},{"NodeType":"BlockStatNode","Line":3,"Children":[{"Type":"Return","NodeType":"JumpStatNode","Line":3,"Children":[{"NodeType":"ExprListNode","Line":3,"Children":[{"Value":1,"Suffix":null,"TypeCode":"Int32","NodeType":"LitExprNode","Line":3,"Children":[]}]}]}]},{"NodeType":"BlockStatNode","Line":5,"Children":[{"Type":"Return","NodeType":"JumpStatNode","Line":5,"Children":[{"NodeType":"ExprListNode","Line":5,"Children":[{"NodeType":"ArithmExprNode","Line":5,"Children":[{"Identifier":"n","NodeType":"IdNode","Line":5,"Children":[]},{"Symbol":"*","NodeType":"ArithmOpNode","Line":5,"Children":[]},{"NodeType":"FuncCallExprNode","Line":5,"Children":[{"Identifier":"fact","NodeType":"IdNode","Line":5,"Children":[]},{"NodeType":"ExprListNode","Line":5,"Children":[{"NodeType":"ArithmExprNode","Line":5,"Children":[{"Identifier":"n","NodeType":"IdNode","Line":5,"Children":[]},{"Symbol":"-","NodeType":"ArithmOpNode","Line":5,"Children":[]},{"Value":1,"Suffix":null,"TypeCode":"Int32","NodeType":"LitExprNode","Line":5,"Children":[]}]}]}]}]}]}]}]}]}]}]}]}]}]}
```
</details>

## Visualizing ASTs:

Visualized AST using [LINVisualizer](https://github.com/LINVAST/LINVisualizer):

![tree](https://raw.githubusercontent.com/LINVAST/LINVisualizer/master/Samples/visualizer.png)


## Comparing ASTs using library extensions

Detailed examples comparing language-invariant ASTs can be seen [here](https://github.com/LINVAST/LINVAST.Imperative.Comparers).


### Sample comparing output for two C implementations of function swap

![swap](https://raw.githubusercontent.com/LINVAST/LINVAST.Imperative.Comparers/master/Samples/swap/valid_c-wrong_c.PNG)


# Extending AST library with new programming languages

Steps (for imperative language, in other cases only the namespace is different):
- Create (or use ![already made](https://github.com/antlr/grammars-v4/)) ANTLR4 grammar files
- Create lexer and parser for your grammar via ANTLR4 (using `-Dlanguage=CSharp` option)
- Create a builder namespace in `LINVAST.Imperative.Nodes.Builder.<YOUR_GRAMMAR>` as per examples already present for C or Lua
- Create a Builder type extending `ANTLR_GENERATED_BASE_VISITOR<ASTNode>` and implementing `IASTBuilder<ANTLR_GENERATED_PARSER_TYPE>` (change uppercase areas appropriately)
- Apply `[ASTBuilder(".YOUR_FILE_EXTENSION")]` attribute to the class in order for it to be automatically used when loading sources of that extension

Check out the Pseudocode PoC language ![grammar](LINVAST.Imperative/Builders/Pseudo/ANTLR/Pseudo.g4) and ![builder](LINVAST.Imperative/Builders/Pseudo/) as an example.
