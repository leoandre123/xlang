using System;
using System.Collections.Generic;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;


namespace xlang.Compiler.SemanticAnalysis;

public class Scope
{
    private record Entry(Symbol Symbol, bool IsUsed = false);

    public Scope? Parent { get; }

    private readonly Dictionary<string, Entry> _map = new();

    public Scope(Scope? parent)
    {
        Parent = parent;
    }

    public bool TryDeclare(Symbol symbol)
    {
        Debug.Assert(symbol is not CallableSymbol or ClassSymbol);

        if (_map.ContainsKey(symbol.Name)) return false;
        _map[symbol.Name] = new Entry(symbol);
        return true;
    }
    public Symbol? LookupVariable(string name)
    {
        var scope = this;
        while (scope != null)
        {
            if (scope._map.TryGetValue(name, out var entry))
            {
                _map[name] = entry with { IsUsed = true };
                return entry.Symbol;
            }

            scope = scope.Parent;
        }

        return null;
    }
    public void LogUnused()
    {
        foreach (var value in _map.Values.Where(y => !y.IsUsed))
        {
            Logger.LogWarning($"{value.Symbol.GetSymbolTypeName()} '{value.Symbol.Name}' is unused", value.Symbol.OwnerModule.File, value.Symbol.Span);
        }
    }
}
