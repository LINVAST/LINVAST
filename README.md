# LINVAST - Language-INVariant AST library
[![Issues](https://img.shields.io/github/issues/LINVAST/LINVAST.svg)](https://github.com/LINVAST/LINVAST/issues)
[![Stable release](https://img.shields.io/github/release/LINVAST/LINVAST.svg?label=stable)](https://github.com/LINVAST/LINVAST/releases)
[![Latest release](https://img.shields.io/github/tag-pre/LINVAST/LINVAST.svg?label=latest)](https://github.com/LINVAST/LINVAST/releases)
[![NuGet](https://img.shields.io/nuget/vpre/LINVAST.svg?label=NuGet:%20LINVAST)](https://nuget.org/packages/LINVAST)
[![NuGet](https://img.shields.io/nuget/vpre/LINVAST.Imperative.svg?label=NuGet:%20LINVAST.Imperative)](https://nuget.org/packages/LINVAST.Imperative)
[![Stable release](https://img.shields.io/github/release/LINVAST/CLI.svg?label=linvast-cli)](https://github.com/LINVAST/CLI/releases)
## Motivation and project description

LINVAST provides a shared abstraction for analyzing code across programming languages, despite differences in syntax and parse trees. With language-specific adapters (_Builders_) that convert [ANTLR](https://www.antlr.org/) parse trees into a common AST, it supports generating, traversing, and serializing ASTs, along with common operations such as expression evaluation. Created as a proof of concept for an MSc thesis on comparing similar imperative code, LINVAST has grown into a reusable tool and API for applications that visualize or compare code structures.

Currently, the main focus of the project is the imperative programming paradigm, with supported languages (in alphabetical order):

| Language | Grammar File(s) | Version Reference | Grammar Source |
|---|---|---|---|
| C | [`C.g4`](https://github.com/LINVAST/LINVAST/search?q=C.g4&type=code) | C11 | C 2011 grammar based on the C11 specification |
| Go | [`GoParser.g4`](https://github.com/LINVAST/LINVAST/search?q=GoParser.g4&type=code) / [`GoLexer.g4`](https://github.com/LINVAST/LINVAST/search?q=GoLexer.g4&type=code) | Go 1.25 | Derived from the Go language specification |
| Java | [`Java.g4`](https://github.com/LINVAST/LINVAST/search?q=Java.g4&type=code) / [`JavaLexer.g4`](https://github.com/LINVAST/LINVAST/search?q=JavaLexer.g4&type=code) | Java 8 | BSD-licensed ANTLR Java 8 grammar by Ivan Kochurkin |
| Kotlin | [`KotlinParser.g4`](https://github.com/LINVAST/LINVAST/search?q=KotlinParser.g4&type=code) | Kotlin 1.x | Based on Kotlin compiler test data |
| Lua | [`Lua.g4`](https://github.com/LINVAST/LINVAST/search?q=Lua.g4&type=code) | Lua 5.1–5.3 | Multi-version grammar tested against Lua 5.2/5.3 test suites |
| Python | [`Python3Parser.g4`](https://github.com/LINVAST/LINVAST/search?q=Python3Parser.g4&type=code) / [`Python3Lexer.g4`](https://github.com/LINVAST/LINVAST/search?q=Python3Lexer.g4&type=code) | Python 3 | ANTLR4 Python 3 grammar by Bart Kiers |


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

Visualized AST using [LINVisualizerJS](https://github.com/LINVAST/LINVisualizerJS):

![tree](https://raw.githubusercontent.com/LINVAST/LINVisualizerJS/refs/heads/main/images/showcase.png)


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
