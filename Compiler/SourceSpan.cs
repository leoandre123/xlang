namespace xlang.Compiler;

public record SourceSpan(int Start, int Length)
{
    public static SourceSpan FromStartAndEnd(int start, int end) => new(start, end - start);

    public int End => Start + Length;
}