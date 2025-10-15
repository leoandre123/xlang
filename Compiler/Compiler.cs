using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using xlang.Compiler.CodeGeneration;
using xlang.Compiler.Optimization;
using xlang.Compiler.Parsing;
using xlang.Compiler.SemanticAnalysis;
using xlang.Compiler.Structures;
using xlang.Compiler.Structures.AST;
using xlang.Compiler.Tokenization;
using xlang.Compiler.Utils;

namespace xlang.Compiler;

public record LinkerSettings
{
    public required string LinkerPath { get; init; }
    public required bool Debug { get; init; }
    public required string EntryPoint { get; init; }
    public required string OutputName { get; init; }
}

public record AssemblerSettings
{
    public required string AssemblerPath { get; init; }
}

public static class Compiler
{


    private static SemanticAnalyzer tmpAnalyzer;

    public static bool Build(string[] files, CompilerSettings settings)
    {
        var sw = Stopwatch.StartNew();
        Logger.LogInfo($"========== Build started at {DateTime.Now:HH:mm} ==========");

        var success = true;
        try
        {
            success = RunBuild(files, settings);
        }
        catch (Exception e)
        {
            var msg = "";
            if (e is CompilationException ce)
            {
                var source = File.ReadAllText(ce.SourceFile);
                var srcLoc = GetSourceLocation(source, ce.Span.Start);
                msg = $"{e.GetType().Name}: {ce.Message}\nat {srcLoc.line}:{srcLoc.column}\n\n{string.Join('\n', GetErrorLines(source, ce.Span))}";
            }
            else
            {
                msg = e.Message;
            }

            if (tmpAnalyzer != null)
                PrintScopeTree(tmpAnalyzer.GetModel().ScopeTreeBaseNode);

            Logger.LogError(msg);
            success = false;
        }


        sw.Stop();

        Logger.LogInfo($"========== Build {(success ? "finished" : "failed")} at {DateTime.Now:HH:mm} and took {sw.ElapsedMilliseconds} ms ==========");
        return success;
    }

    private static bool RunBuild(string[] files, CompilerSettings settings)
    {
        List<string> sources = [];
        List<string> asmFiles = [];
        List<string> objFiles = [];

        List<Module> modules = [];

        foreach (var file in files)
        {
            var source = File.ReadAllText(file);
            sources.Add(source);

            var lexer = new Lexer(source, file);
            var tokens = lexer.Tokenize();

            var parser = new Parser(tokens, file);
            var module = parser.ParseProgram();

            if (settings.PrintAst)
                AstPrinter.PrintAST(module);

            modules.Add(module);
        }

        var analyzer = new SemanticAnalyzer(modules);
        tmpAnalyzer = analyzer;
        var model = analyzer.Analyze();


        LogWarnings(model.Diagnostics, modules);

        if (settings.PrintScopeTree)
            PrintScopeTree(model.ScopeTreeBaseNode);

        for (var i = 0; i < modules.Count; i++)
        {
            //var prog = modules[i].Program;
            //var optimizer = new AstOptimizer();
            //var optimizedProg = optimizer.Optimize(prog);

            var generator = new CodeGenerator(modules[i], model, sources[i]);
            var asm = generator.Generate();

            var outputPath = Path.ChangeExtension(files[i], "asm");
            File.WriteAllText(outputPath, asm);
            asmFiles.Add(outputPath);
            if (settings.PrintAssembly)
                PrintAssembly(outputPath);
        }

        foreach (var asmFile in asmFiles)
        {
            objFiles.Add(AssembleFile(asmFile, new AssemblerSettings { AssemblerPath = "C:\\Program Files\\NASM\\nasm.exe" }));
        }

        LinkFiles(objFiles.ToArray(), new LinkerSettings
        {
            Debug = settings.Debug,
            OutputName = settings.OutputName,
            LinkerPath = "C:\\Program Files\\Microsoft Visual Studio\\2022\\Community\\VC\\Tools\\MSVC\\14.42.34433\\bin\\HostX64\\x64\\link.exe",
            EntryPoint = Utils.Utils.SanitizeFqn(model.GetEntryPoint()!.FullyQualifiedName)
        });

        Logger.LogInfo($"Build -> {settings.OutputName}");
        return true;
    }

    private static string AssembleFile(string inputAsmFile, AssemblerSettings settings)
    {
        var asmFull = Path.GetFullPath(inputAsmFile);
        var baseName = Path.GetFileNameWithoutExtension(asmFull);
        var obj = Path.Combine(baseName + ".obj");
        var lst = Path.Combine(baseName + ".lst");

        var nasmArgs = new List<string> {
            "-f", "win64","-g", "-F", "cv8"
        };

        nasmArgs.AddRange([asmFull, "-o", obj]);
        nasmArgs.AddRange(["-l", lst]);

        RunTool(settings.AssemblerPath, nasmArgs, Directory.GetCurrentDirectory(), $"NASM {baseName}");

        return obj;
    }
    private static void LinkFiles(string[] objFiles, LinkerSettings settings)
    {
        var rspPath = Path.Combine($"link_{DateTime.UtcNow:yyyyMMddHHmmssfff}.rsp");
        List<string> allLibs = ["C:\\Program Files (x86)\\Windows Kits\\10\\Lib\\10.0.22621.0\\um\\x64\\kernel32.lib"];

        var linkFlags = new List<string> {
            "/nologo",
            settings.Debug ? "/debug":"/debug:none",
            "/opt:noref",
            "/opt:noicf",
            "/incremental:no",
            $"/subsystem:console",
            $"/entry:{settings.EntryPoint}",
            $"/out:{Quote(settings.OutputName)}"
        };

        var rspLines = new List<string>();
        rspLines.AddRange(objFiles.Select(Quote));
        rspLines.AddRange(linkFlags);
        rspLines.AddRange(allLibs.Select(Quote));
        File.WriteAllLines(rspPath, rspLines);

        RunTool(settings.LinkerPath, new[] { "@" + rspPath }, Directory.GetCurrentDirectory(), "LINK");
        File.Delete(rspPath);
        Console.WriteLine();
    }

    private static string[] GetErrorLines(string source, SourceSpan span)
    {
        if (source.Length == 0)
            return [];

        var start = Math.Min(span.Start, source.Length - 1);

        var srcLoc = GetSourceLocation(source, start);
        var lines = source.Split(Environment.NewLine);

        if (srcLoc.line == -1)
            return [];

        return [
            srcLoc.line > 0 ? $"{srcLoc.line,4}: {lines[srcLoc.line - 1].Replace('\t', ' ')}" : "",
            $"{srcLoc.line + 1,4}: {lines[srcLoc.line].Replace('\t', ' ')}",
            "^".PadLeft(($"{(srcLoc.line + 1).ToString(),4}: ").Length + srcLoc.column),
        ];
    }

    private static (int line, int column) GetSourceLocation(string source, int pos)
    {
        var line = 0;
        var newline = source.IndexOf('\n');
        var column = pos;
        while (newline < pos)
        {
            column = pos - newline;
            line++;
            newline = source.IndexOf('\n', newline + 1);

            if (newline == -1)
            {
                return (-1, -1);
            }
        }


        return (line, column);
    }

    private static void PrintScopeTree(ScopeNode node, int indent = 0)
    {
        for (var i = 0; i < indent; i++)
        {
            Console.Write(i == indent - 1 ? "|---" : "|   ");
        }

        Console.ForegroundColor = ConsoleColor.Green;
        Console.Write(node.Name);
        Console.ForegroundColor = ConsoleColor.White;
        if (node.ScopeType == EScopeType.Class)
        {
            Console.WriteLine($" (Class Type: {(node.GetSymbolValue() as ClassSymbol)?.Type})");
        }
        else
        {
            Console.WriteLine(" (Scope)");
        }

        var allSymbols = node.GetAllSymbols(false);
        foreach (var symbol in allSymbols)
        {
            for (var i = 0; i < indent + 1; i++)
            {
                Console.Write(i == indent ? "|---" : "|   ");
            }

            Console.ForegroundColor = ConsoleColor.DarkMagenta;
            Console.Write(symbol.Name);
            Console.ForegroundColor = ConsoleColor.White;
            Console.WriteLine($" ({symbol.GetSymbolTypeName()}{(symbol is SymbolWithType swt ? $" Type: {swt.Type}" : "")})");
        }

        foreach (var child in node.Children)
        {
            PrintScopeTree(child, indent + 1);
        }
    }

    private static void PrintAssembly(string file)
    {
        var lines = File.ReadAllLines(file);
        Console.WriteLine(file);
        for (var i = 0; i < lines.Length; i++)
        {
            var line = lines[i];
            Console.ForegroundColor =
                line.TrimStart().StartsWith(';') ? ConsoleColor.Blue :
                line.TrimEnd().EndsWith(':') ? ConsoleColor.DarkRed :
                line.StartsWith(' ') ? ConsoleColor.White :
                ConsoleColor.Green;
            Console.WriteLine($"{i + 1,3}: {line}");
        }
        Console.ForegroundColor = ConsoleColor.White;
    }

    private static void RunTool(string exe, IEnumerable<string> args, string workingDir, string tag)
    {
        var psi = new ProcessStartInfo
        {
            FileName = exe,
            WorkingDirectory = workingDir,
            UseShellExecute = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            CreateNoWindow = true,
            Arguments = string.Join(" ", args.Select(s => s.Contains(' ') ? $"\"{s}\"" : s))
        };

        using var proc = Process.Start(psi) ?? throw new InvalidOperationException($"Failed to start {exe}");
        var stdout = proc.StandardOutput.ReadToEnd();
        var stderr = proc.StandardError.ReadToEnd();
        proc.WaitForExit();

        if (proc.ExitCode != 0)
        {
            var message = $"[{tag}] exited with code {proc.ExitCode}\n--- stdout ---\n{stdout}\n--- stderr ---\n{stderr}";
            throw new Exception(message);
        }

        if (!string.IsNullOrWhiteSpace(stdout)) Console.WriteLine(stdout);
        if (!string.IsNullOrWhiteSpace(stderr)) Console.WriteLine(stderr);
    }
    private static string Quote(string s) => $"\"{s}\"";

    private static void LogWarnings(List<Diagnostic> diagnostics, List<Module> modules)
    {
        foreach (var diagnostic in diagnostics)
        {
            var module = modules.Single(x => x.Id == diagnostic.ModuleId);
            Logger.LogWarning($"{module.File}({diagnostic.Span.Start}, {diagnostic.Span.End}): {diagnostic.Message}");
        }
    }
}