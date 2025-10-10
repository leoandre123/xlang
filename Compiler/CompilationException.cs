namespace xlang.Compiler;

public class CompilationException(string message, SourceSpan span, string sourceFile) : Exception(message)
{
    public string SourceFile { get; } = sourceFile;
    public SourceSpan Span { get; } = span;
}