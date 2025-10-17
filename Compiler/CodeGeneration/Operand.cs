using System.Diagnostics;
using System.Drawing;
using xlang.Compiler.Utils;

namespace xlang.Compiler.CodeGeneration;

public abstract record Operand(int Size)
{
    public abstract string AsmName { get; }
}

public record RegisterOperand(string Name, int Size) : Operand(Size)
{
    public bool IsXmm { get; init; } = Name.StartsWith("xmm");
    public bool IsGpr { get; init; } = !Name.StartsWith("xmm");
    public override string AsmName => AssemblyUtils.GetRegister(Name, Size);

    public static readonly RegisterOperand Rax = new("rax", 8);
    public static readonly RegisterOperand Rcx = new("rcx", 8);
    public static readonly RegisterOperand Rdx = new("rdx", 8);

    public static readonly RegisterOperand Rbp = new("rbp", 8);
    public static readonly RegisterOperand Rsp = new("rsp", 8);

    public static readonly RegisterOperand R8 = new("r8", 8);
    public static readonly RegisterOperand R9 = new("r9", 8);
    public static readonly RegisterOperand R10 = new("r10", 8);
    public static readonly RegisterOperand R11 = new("r11", 8);


    public static readonly RegisterOperand Xmm0 = new("xmm0", 8);
    public static readonly RegisterOperand Xmm1 = new("xmm1", 8);
    public static readonly RegisterOperand Xmm2 = new("xmm2", 8);

    public static RegisterOperand FromRegister(Register idxReg)
    {
        return new RegisterOperand(idxReg.Name, 8);
    }
}
public record MemoryOperand : Operand
{
    public MemoryOperand(string location, int size) : base(size)
    {
        var reg = "";
        var offset = "0";
        var offsetMode = false;

        Debug.Assert(location.StartsWith('[') && location.EndsWith(']'));

        location = location.TrimStart('[').TrimEnd(']');

        if (location.StartsWith("rel "))
        {
            IsRel = true;
            location = location[4..];

        }
        foreach (var c in location)
        {
            if (offsetMode)
            {
                if (char.IsDigit(c))
                    offset += c;
            }
            else
            {
                if (char.IsLetterOrDigit(c))
                    reg += c;

                if (c == '-')
                {
                    offsetMode = true;
                    offset = "-";
                }
                if (c == '+')
                {
                    offsetMode = true;
                    offset = "";
                }
            }
        }
        if (IsRel)
            Label = reg;
        else
            BaseRegister = reg;
        Offset = int.Parse(offset);

    }

    private MemoryOperand(int size) : base(size)
    {

    }

    public static MemoryOperand FromRegister(RegisterOperand baseReg, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = baseReg.Name,
        };
    }
    public static MemoryOperand FromIndex(RegisterOperand baseReg, RegisterOperand indexReg, int scale, int displacement, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = baseReg.Name,
            IndexRegister = indexReg.Name,
            Scale = scale,
            Offset = displacement
        };
    }
    public static MemoryOperand FromOffset(RegisterOperand baseReg, int offset, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = baseReg.AsmName,
            Offset = offset,
        };
    }
    public static MemoryOperand FromOffset(string reg, int offset, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = reg,
            Offset = offset,
        };
    }
    public static MemoryOperand FromLabel(string label, int size)
    {
        return new MemoryOperand(size)
        {
            IsRel = true,
            Label = label,
        };
    }

    public bool IsRel { get; init; }
    public string Label { get; init; }
    public string? BaseRegister { get; init; }
    public string? IndexRegister { get; init; }
    public int Scale { get; init; }
    public int Offset { get; init; }

    public override string AsmName => GetName();

    private string GetName()
    {
        if (IsRel) return $"[rel {Label}]";

        var scalePart = Scale != 1 ? $"*{Scale}" : "";
        var indexPart = IndexRegister != null ? $"+{IndexRegister}{scalePart}" : "";
        var offsetPart = Offset != 0 ? $"{(Offset > 0 ? $"+{Offset}" : Offset)}" : "";

        return $"[{BaseRegister}{indexPart}{offsetPart}]";
    }

    public MemoryOperand WithOffset(int offset, int newSize)
    {
        return this with
        {
            Offset = Offset + offset,
            Size = newSize,
        };
    }
}
public record ImmediateOperand(int Value, int Size) : Operand(Size)
{
    public override string AsmName => $"{Value}";

    public static readonly ImmediateOperand Zero = new(0, 8);
}
