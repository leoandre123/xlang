using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using xlang.Compiler.Structures;

namespace xlang.Compiler.SemanticAnalysis;

public record TypeSymbol
 (string Name, int Size, int Alignment, string FullyQualifiedName, bool IsFloat, bool IsReferenceType)
{
    public string Name { get; set; } = Name;
    public int Size { get; set; } = Size;
    public int Alignment { get; set; } = Alignment;
    public string FullyQualifiedName { get; set; } = FullyQualifiedName;
    public bool IsFloat { get; set; } = IsFloat;

    public override string ToString() => Name;
    public virtual bool Equals(TypeSymbol? other)
    {
        if (GetType() == other?.GetType())
        {
            return other?.Size == Size && other?.Alignment == Alignment;
        }
        return false;
    }
}

public record PrimitiveTypeSymbol(string Name, int Size, int Alignment, bool IsSigned, bool IsFloat) : TypeSymbol(Name, Size, Alignment, $"System.{Name}", IsFloat, false)
{
    public override string ToString() => Name;
    public bool IsSigned { get; set; } = IsSigned;
    public virtual bool Equals(PrimitiveTypeSymbol? other)
    {
        if (GetType() == other?.GetType())
        {
            return base.Equals(other) && other?.IsSigned == IsSigned && other?.IsFloat == IsFloat;
        }
        return false;
    }
}

public record UnresolvedTypeSymbol(string Name) : TypeSymbol(Name, -1, -1, "", false, false)
{
    public override string ToString() => $"{Name} (Unresolved)";
}

public record ClassTypeSymbol(string Name, int ClassSize, int ClassAlignment, string FullyQualifiedName) : TypeSymbol(Name, 8, 8, FullyQualifiedName, false, true)
{
    public ClassSymbol DeclaringSymbol { get; set; }
    public Dictionary<SymbolId, int> FieldOffset { get; init; } = [];
    public override string ToString() => Name;
}

public record PointerTypeSymbol(TypeSymbol Type) : TypeSymbol($"{Type.Name}*", 8, 8, $"{Type.FullyQualifiedName}*",false, false);

public record ArrayTypeSymbol(TypeSymbol Type) : TypeSymbol($"{Type.Name}[]", Type.Size, Type.Alignment, $"{Type.Name}[]",false, false);

public record ScopeTypeSymbol(string Name, string FullyQualifiedName) : TypeSymbol(Name, -1, -1, FullyQualifiedName, false, false);


public static class Types
{
    public static readonly PrimitiveTypeSymbol Int = new("int", 4, 4, true, false);
    public static readonly PrimitiveTypeSymbol Long = new("long", 8, 8, true, false);
    public static readonly PrimitiveTypeSymbol Short = new("short", 2, 2, true, false);
    public static readonly PrimitiveTypeSymbol Byte = new("byte", 1, 1, true, false);

    public static readonly PrimitiveTypeSymbol UInt = new("uint", 4, 4, false, false);
    public static readonly PrimitiveTypeSymbol ULong = new("ulong", 8, 8, false, false);
    public static readonly PrimitiveTypeSymbol UShort = new("ushort", 2, 2, false, false);
    public static readonly PrimitiveTypeSymbol UByte = new("ubyte", 1, 1, false, false);

    public static readonly PrimitiveTypeSymbol Float = new("float", 4, 4, true, true);
    public static readonly PrimitiveTypeSymbol Double = new("double", 8, 8, true, true);

    public static readonly PrimitiveTypeSymbol Bool = new("bool", 1, 1, false, false);

    public static readonly TypeSymbol Void = new("void", 0, -1, "void", false, false);

    //public static readonly TypeSymbol String = new ClassTypeSymbol("string", 16, 8, "System.string");

    public static readonly List<TypeSymbol> BuiltIn = [

        Int    ,
        Long   ,
        Short  ,
        Byte   ,

        UInt   ,
        ULong  ,
        UShort ,
        UByte  ,

        Float  ,
        Double ,

        Bool   ,
        Void   ,

        //String

    ];


    public static readonly List<TypeSymbol> IntegerTypes = [

        Int    ,
        Long   ,
        Short  ,
        Byte   ,

        UInt   ,
        ULong  ,
        UShort ,
        UByte
    ];
    public static readonly List<TypeSymbol> FloatingPointTypes = [

        Float,
        Double,
    ];

    public static bool IsIntegerType(TypeSymbol type) => IntegerTypes.Contains(type);

    public static bool IsBuiltInConversionAllowed(TypeSymbol to, TypeSymbol from)
    {
        if (IntegerTypes.Contains(to) && IntegerTypes.Contains(from)) return true;
        if (FloatingPointTypes.Contains(to) && IntegerTypes.Contains(from)) return true;
        if (FloatingPointTypes.Contains(from) && IntegerTypes.Contains(to)) return true;
        if (to is PointerTypeSymbol && IntegerTypes.Contains(from)) return true;
        if (to is PointerTypeSymbol && from is PointerTypeSymbol) return true;

        return false;
    }

    public static bool IsImplicitConversionAllowed(TypeSymbol to, TypeSymbol from)
    {
        if (to is PointerTypeSymbol ptrTo && from is PointerTypeSymbol ptrFrom)
        {
            IsImplicitConversionAllowed(ptrTo.Type, ptrFrom.Type);
        }
        if (to is PrimitiveTypeSymbol primTo && from is PrimitiveTypeSymbol primFrom)
        {
            if (primTo.IsFloat != primFrom.IsFloat) return false;
            if (primTo.Size < primFrom.Size) return false;
            return true;
        }

        return false;
    }

    public static TypeSymbol? GetCommonTypeSymbol(TypeSymbol a, TypeSymbol b)
    {
        if (a.IsFloat || b.IsFloat)
            return a == Double || b == Double ? Double : Float;

        if (IsIntegerType(a) && IsIntegerType(b))
        {
            return a.Size > b.Size ? a : b;
        }

        return null;
        throw new NotSupportedException($"No common type for {a} and {b}");
    }
}


