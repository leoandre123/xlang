using System.Collections.Generic;
using System.Diagnostics.CodeAnalysis;
using System.IO;
using xlang.Compiler.SemanticAnalysis;
using xlang.Compiler.Structures.AST;

namespace xlang.Compiler.Structures;


//public record ClassMetaData(int Size, IReadOnlyList<ClassMember> Members);
public record FunctionMetaData
{
    public required string FullyQualifiedName { get; init; }
    public required int LocalsSize { get; init; }
    public required bool IsLeaf { get; init; }
}
//public record MethodMetaData(int Size, string FullName);
public enum ValueCategory { RValue, LValue /*ModifiableLValue*/}
public record ExpressionInfo(TypeSymbol Type, ValueCategory Category);
public record ConcatenationList(List<Expression> List);


public abstract record LiteralConstant();
public record StringConstant(string Text) : LiteralConstant;
public record FloatConstant(float Value) : LiteralConstant;

public class SemanticModel
{
    public List<Diagnostic> Diagnostics { get; } = new();
    public Dictionary<int, ConcatenationList> ConcatenationList { get; } = new();
    public Dictionary<int, ExpressionInfo> ExpressionData { get; } = new();
    public Dictionary<int, List<SymbolId>> ExternalFunctions { get; } = new();

    public SymbolId? EntryPoint { get; set; } = null;
    public ScopeNode ScopeTreeBaseNode { get; set; }

    //Symbol table - Maps node ids -> symbols
    public Dictionary<SymbolId, Symbol> Symbols { get; } = new();
    public Dictionary<int, SymbolId> NodeToSymbolId { get; } = new();

    public Dictionary<SymbolId, FunctionMetaData> FunctionData { get; } = new();

    public Dictionary<int, int> LiteralConstantMap { get; } = new();
    public List<LiteralConstant> LiteralConstants { get; } = new();



    public SemanticModel()
    {
        ScopeTreeBaseNode = new(EScopeType.NamedScope, null, null, Symbols);
    }


    public T GetSymbol<T>(int nodeId) where T : Symbol
    {
        return Symbols[NodeToSymbolId[nodeId]] as T;
    }
    public T GetSymbol<T>(SymbolId symbolId) where T : Symbol
    {
        return Symbols[symbolId] as T;
    }
    public Symbol GetSymbol(int nodeId)
    {
        return Symbols[NodeToSymbolId[nodeId]];
    }
    public Symbol GetSymbol(SymbolId symbolId)
    {
        return Symbols[symbolId];
    }

    public FunctionSymbol? GetEntryPoint()
    {
        if (EntryPoint != null) return Symbols[EntryPoint] as FunctionSymbol;
        return null;
    }
    public CallableSymbol GetCallable(CallExpression expression)
    {
        if (NodeToSymbolId.TryGetValue(expression.Id, out var symId))
            if (Symbols.TryGetValue(symId, out var sym))
                return sym is CallableSymbol cs ? cs : throw new Exception("WTF");

        throw new Exception("");
    }
}