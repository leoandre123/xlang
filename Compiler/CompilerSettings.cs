namespace xlang.Compiler;


public record CompilerSettings
{
    public required string OutputName { get; init; }
    public bool Debug { get; init; } = false;
    public bool PrintAssembly { get; init; } = false;
    public bool PrintAst { get; init; } = false;
    public bool PrintScopeTree { get; init; } = false;
}
