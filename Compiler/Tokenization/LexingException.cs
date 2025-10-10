namespace xlang.Compiler.Tokenization;

public class LexingException(string message, SourceSpan span, string sourceFile) : CompilationException(message, span, sourceFile);