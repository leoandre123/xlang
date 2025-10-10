namespace xlang.Compiler.Structures.AST;

public record BoundNode(SourceSpan Span)
{
    public int Id { get; } = GenerateId();

    private static int _currentId = 0;
    private static int GenerateId()
    {
        return _currentId++;
    }
}

