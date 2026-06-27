using System;
using System.IO;
using LINVAST.Imperative.Builders.Kotlin;
using LINVAST.Nodes;

if (args.Length == 0) {
    Console.WriteLine("Usage: dotnet run --project LINVAST.Runner <path-to-kotlin-file>");
    Console.WriteLine();
    Console.WriteLine("Examples:");
    Console.WriteLine("  dotnet run --project LINVAST.Runner samples/valid/functions.kt");
    Console.WriteLine("  dotnet run --project LINVAST.Runner samples/valid/expressions.kt");
    Console.WriteLine("  dotnet run --project LINVAST.Runner samples/unsupported/for_loop.kt");
    return;
}

string path = args[0];

if (!File.Exists(path)) {
    Console.WriteLine($"File not found: {path}");
    return;
}

Console.WriteLine($"Parsing: {path}");
Console.WriteLine(new string('-', 50));

try {
    string code = File.ReadAllText(path);
    ASTNode ast = new KotlinASTBuilder().BuildFromSource(code);
    PrintNode(ast, 0);
    Console.WriteLine();
    Console.WriteLine("Parsed successfully.");
} catch (NotImplementedException ex) {
    Console.WriteLine($"Not supported: {ex.Message}");
} catch (Exception ex) {
    Console.WriteLine($"Error: {ex.Message}");
}

static void PrintNode(ASTNode node, int depth)
{
    string indent = new string(' ', depth * 2);
    Console.WriteLine($"{indent}{node.GetType().Name}");
    foreach (ASTNode child in node.Children)
        PrintNode(child, depth + 1);
}
