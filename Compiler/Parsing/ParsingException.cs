namespace xlang.Compiler.Parsing;

public class ParsingException(string message, SourceSpan span, string sourceFile) : CompilationException(message, span, sourceFile);