using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using xlang.Compiler.SemanticAnalysis;

namespace xlang.Compiler.Structures;


public enum EScopeType
{
    NamedScope,
    Class
}

public class ScopeNode(EScopeType type, string? name, ScopeNode? parent, Dictionary<SymbolId, Symbol> allSymbols)
{
    public EScopeType ScopeType { get; private init; } = type;
    public string? Name { get; private init; } = name;
    public ScopeNode? Parent { get; private init; } = parent;
    public SymbolId? Symbol { get; private set; } = null;
    public List<ScopeNode> Children { get; } = [];
    public List<SymbolId> Symbols { get; } = [];

    private Dictionary<SymbolId, Symbol> _allSymbols = allSymbols;

    public string GetFullName(string? child = null)
    {
        List<string> names = child == null ? [] : [child];
        var node = this;
        while (node != null)
        {
            if (node.Name != null)
                names.Insert(0, node.Name);
            node = node.Parent;
        }

        return string.Join('.', names);
    }
    public ScopeNode AddClassChild(string name)
    {
        Debug.Assert(!name.Contains('.'));


        var child = new ScopeNode(EScopeType.Class, name, this, _allSymbols);

        Children.Add(child);


        return child;
    }

    public void SetSymbol(SymbolId symbolId)
    {
        Symbol = symbolId;
    }
    public ScopeNode AddNamedScopeChild(string name)
    {
        var parts = name.Split('.');
        var node = this;
        foreach (var part in parts)
        {
            var child = new ScopeNode(EScopeType.NamedScope, part, node, _allSymbols);
            node.Children.Add(child);
            node = child;
        }
        return node;
    }
    public ScopeNode GetOrCreate(string? path)
    {
        if (path == null)
            return this;

        var parts = path.Split('.');

        var node = this;

        foreach (var part in parts)
        {
            var child = node.Children.FirstOrDefault(x => x.Name == part);
            node = child ?? node.AddNamedScopeChild(part);

        }

        return node;
    }
    public ScopeNode? GetNode(string path)
    {
        var parts = path.Split('.');

        var node = this;

        foreach (var part in parts)
        {
            var child = node.Children.FirstOrDefault(x => x.Name == part);
            if (child == null)
                return null;
            node = child;
        }

        return node;
    }


    public (string fqn, Symbol symbol)? GetSymbol(string name)
    {
        var parts = name.Split('.');
        var node = this;

        for (var i = 0; i < parts.Length - 1; i++)
        {
            var child = node.Children.FirstOrDefault(x=>x.Name == parts[i]);
            if (child == null)
                return null;
            node = child;
        }


        var fqn = node.GetFullName() + "." + parts.Last();

        var symbolId =
            node.Symbols.SingleOrDefault(x => _allSymbols[x] is SymbolWithType swt && swt.Name == parts.Last());

        if (symbolId != null) { return (fqn, _allSymbols[symbolId]); }

        symbolId = node.Children
            .SingleOrDefault(x => x.Symbol != null && _allSymbols[x.Symbol] is { } s && s.Name == parts.Last())?.Symbol;
        if (symbolId != null) { return (fqn, _allSymbols[symbolId]); }

        return null;

    }
    public (string fqn, Symbol symbol)? FindSymbolUp(string name)
    {
        for (var node = this; node != null; node = node.Parent)
        {
            var sym = node.GetSymbol(name);
            if (sym != null)
                return sym;
        }

        return null;
    }

    public IReadOnlyList<Symbol> GetAllSymbols()
    {
        List<Symbol> allSymbols = [];
        if (Symbol != null)
            allSymbols.Add(_allSymbols[Symbol]);
        allSymbols.AddRange(Symbols.Select(x => _allSymbols[x]));

        foreach (var child in Children)
        {
            allSymbols.AddRange(child.GetAllSymbols());
        }

        return allSymbols;
    }

    public Symbol GetSymbolValue()
    {
        return _allSymbols[Symbol!];
    }
}