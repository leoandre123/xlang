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


    public (string fqn, Symbol symbol)? GetSymbol(string name, SymbolType type = SymbolType.All)
    {
        var parts = name.Split('.');
        var node = this;

        for (var i = 0; i < parts.Length - 1; i++)
        {
            var child = node.Children.FirstOrDefault(x => x.Name == parts[i]);
            if (child == null)
                return null;
            node = child;
        }

        var fqn = node.GetFullName() + "." + parts.Last();

        var symbolId = type switch
        {
            SymbolType.All => node.Symbols.SingleOrDefault(x => _allSymbols[x].Name == parts.Last()),
            SymbolType.TypeDeclaring => node.Symbols.SingleOrDefault(x => _allSymbols[x] is (ClassSymbol or StructSymbol or AliasSymbol) && _allSymbols[x].Name == parts.Last()),
            SymbolType.Callable => node.Symbols.SingleOrDefault(x => _allSymbols[x] is CallableSymbol && _allSymbols[x].Name == parts.Last()),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };


        if (symbolId != null) { return (fqn, _allSymbols[symbolId]); }

        symbolId = node.Children.SingleOrDefault(x => x.Symbol != null && _allSymbols[x.Symbol] is { } s && s.Name == parts.Last())?.Symbol;

        symbolId = type switch
        {
            SymbolType.All => node.Children.SingleOrDefault(x => x.Symbol != null && _allSymbols[x.Symbol].Name == parts.Last())?.Symbol,
            SymbolType.TypeDeclaring => node.Children.SingleOrDefault(x => x.Symbol != null && _allSymbols[x.Symbol] is (ClassSymbol or StructSymbol or AliasSymbol) && _allSymbols[x.Symbol].Name == parts.Last())?.Symbol,
            SymbolType.Callable => node.Children.SingleOrDefault(x => x.Symbol != null && _allSymbols[x.Symbol] is CallableSymbol && _allSymbols[x.Symbol].Name == parts.Last())?.Symbol,
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        };

        if (symbolId != null) { return (fqn, _allSymbols[symbolId]); }

        return null;

    }
    public List<(string fqn, Symbol symbol)> GetManySymbols(string name, SymbolType type = SymbolType.All)
    {
        List<(string fqn, Symbol symbol)> list = [];

        var parts = name.Split('.');
        var node = this;

        for (var i = 0; i < parts.Length - 1; i++)
        {
            var child = node.Children.FirstOrDefault(x => x.Name == parts[i]);
            if (child == null)
                return [];
            node = child;
        }

        var fqn = node.GetFullName() + "." + parts.Last();

        var symbolIds = (type switch
        {
            SymbolType.All => node.Symbols.Where(x => _allSymbols[x].Name == parts.Last()),
            SymbolType.TypeDeclaring => node.Symbols.Where(x =>
                _allSymbols[x] is (ClassSymbol or StructSymbol or AliasSymbol) && _allSymbols[x].Name == parts.Last()),
            SymbolType.Callable => node.Symbols.Where(x =>
                _allSymbols[x] is CallableSymbol && _allSymbols[x].Name == parts.Last()),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        }).ToList();

        symbolIds.AddRange(type switch
        {
            SymbolType.All => node.Children.Where(x => x.Symbol != null && _allSymbols[x.Symbol].Name == parts.Last()).Select(y => y.Symbol),
            SymbolType.TypeDeclaring => node.Children.Where(x => x.Symbol != null && _allSymbols[x.Symbol] is (ClassSymbol or StructSymbol or AliasSymbol) && _allSymbols[x.Symbol].Name == parts.Last()).Select(y => y.Symbol),
            SymbolType.Callable => node.Children.Where(x => x.Symbol != null && _allSymbols[x.Symbol] is CallableSymbol && _allSymbols[x.Symbol].Name == parts.Last()).Select(y => y.Symbol),
            _ => throw new ArgumentOutOfRangeException(nameof(type), type, null)
        });

        return symbolIds.Select(x => (fqn, _allSymbols[x])).ToList() ?? [];
    }
    public (string fqn, Symbol symbol)? FindSymbolUp(string name, SymbolType type)
    {
        for (var node = this; node != null; node = node.Parent)
        {
            var sym = node.GetSymbol(name, type);
            if (sym != null)
                return sym;
        }

        return null;
    }

    public List<(string fqn, Symbol symbol)> FindManySymbolsUp(string name, SymbolType type)
    {
        List<(string fqn, Symbol symbol)> list = [];

        for (var node = this; node != null; node = node.Parent)
        {
            var sym = node.GetManySymbols(name, type);
            list.AddRange(sym);
        }

        return list;
    }

    //public (string fqn, Symbol symbol)? FindTypeDeclaringSymbolUp(string name)
    //{
    //    for (var node = this; node != null; node = node.Parent)
    //    {
    //        var sym = node.GetSymbol(name);
    //        if (sym?.symbol is (ClassSymbol or StructSymbol or AliasSymbol))
    //            return sym;
    //    }
    //
    //    return null;
    //}
    //public (string fqn, Symbol symbol)? FindCallableSymbolUp(string name)
    //{
    //    for (var node = this; node != null; node = node.Parent)
    //    {
    //        var sym = node.GetSymbol(name);
    //        if (sym?.symbol is (CallableSymbol))
    //            return sym;
    //    }
    //
    //    return null;
    //}

    public IReadOnlyList<Symbol> GetAllSymbols(bool checkChildren)
    {
        List<Symbol> allSymbols = [];
        if (Symbol != null)
            allSymbols.Add(_allSymbols[Symbol]);
        allSymbols.AddRange(Symbols.Select(x => _allSymbols[x]));

        if (checkChildren)
            foreach (var child in Children)
            {
                allSymbols.AddRange(child.GetAllSymbols(checkChildren));
            }

        return allSymbols;
    }

    public Symbol GetSymbolValue()
    {
        return _allSymbols[Symbol!];
    }
}