namespace xlang.Compiler.Structures;

public enum DiagnosticCode
{
    UnusedLocal,
    UnusedParameter,
    UnusedField,
    UnusedClass,
    UnusedImport,
    UnusedAlias,
    UnusedFunction,

    NeverReadLocal,
    NeverReadParameter,
    NeverReadField,

    AlwaysFalse,
    AlwaysTrue,

    NeverRun,
    RunForever,


    UnreachableCode,

}

public enum DiagnosticSeverity
{
    Warning
}

public record Diagnostic
{
    public required DiagnosticCode Code { get; init; }
    public required DiagnosticSeverity Severity { get; init; }
    public required string Message { get; init; }
    public required int ModuleId { get; init; }
    public required SourceSpan Span { get; init; }
}