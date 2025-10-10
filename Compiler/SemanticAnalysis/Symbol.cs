using xlang.Compiler.Structures;
using xlang.Compiler.Structures.AST;

namespace xlang.Compiler.SemanticAnalysis;

public record SymbolId
{
    public static int NextId = 0;
    public int Id = NextId++;
}

public abstract record Symbol
{
    public required string Name { get; init; }
    public SymbolId Id { get; } = new();
    public required Module OwnerModule { get; init; }
    public required SourceSpan Span { get; init; }

    public string GetSymbolTypeName() => this switch
    {
        ConstructorSymbol => "Constructor",
        ExternalFunctionSymbol => "Extern function",
        MethodSymbol => "Method",
        FunctionSymbol => "Function",
        ClassSymbol => "Class",
        FieldSymbol => "Field",
        ParameterSymbol => "Parameter",
        VariableSymbol => "Variable",
        _ => "Unknown type"
    };
}

public record ScopeSymbol : Symbol
{
    public required string FullyQualifiedName { get; init; }
}

public record SymbolWithType : Symbol
{
    public required TypeSymbol Type { get; init; }
}
public record VariableSymbol : SymbolWithType
{
    public required int Offset { get; init; }
}

public record ParameterSymbol : SymbolWithType
{
    public required int Index { get; init; }
}

public record AliasSymbol : SymbolWithType;

public abstract record CallableSymbol : SymbolWithType
{
    public required string FullyQualifiedName { get; init; }
    public required IReadOnlyList<SymbolId> Parameters { get; init; }
}
public record ExternalFunctionSymbol : CallableSymbol;
public record FunctionSymbol : CallableSymbol
{
    public required EAccessType AccessType { get; init; }
}

public record MethodSymbol : FunctionSymbol;

public record ConstructorSymbol : FunctionSymbol;


public record FieldSymbol : SymbolWithType
{
    public required int Offset { get; init; }
}

public record ClassSymbol : SymbolWithType
{
   public required IReadOnlyList<SymbolId> Members { get; init; }
}