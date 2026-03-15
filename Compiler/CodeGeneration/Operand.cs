using System.Diagnostics;
using System.Drawing;
using xlang.Compiler.Utils;

namespace xlang.Compiler.CodeGeneration;

public abstract record Operand(int Size)
{
    public abstract string AsmName { get; }
}

public record RegisterOperand(Register Register, int Size) : Operand(Size)
{
    public bool IsXmm => Register.IsXmm;
    public bool IsGpr => Register.IsGpr;
    public override string AsmName => AssemblyUtils.GetRegisterName(Register, Size);

    public static readonly RegisterOperand Rax = new(Register.Rax, 8);
    public static readonly RegisterOperand Rcx = new(Register.Rcx, 8);
    public static readonly RegisterOperand Rdx = new(Register.Rdx, 8);

    public static readonly RegisterOperand Rbp = new(Register.Rbp, 8);
    public static readonly RegisterOperand Rsp = new(Register.Rsp, 8);

    public static readonly RegisterOperand R8 = new(Register.R8, 8);
    public static readonly RegisterOperand R9 = new(Register.R9, 8);
    public static readonly RegisterOperand R10 = new(Register.R10, 8);
    public static readonly RegisterOperand R11 = new(Register.R11, 8);


    public static readonly RegisterOperand Xmm0 = new(Register.Xmm0, 8);
    public static readonly RegisterOperand Xmm1 = new(Register.Xmm1, 8);
    public static readonly RegisterOperand Xmm2 = new(Register.Xmm2, 8);
    public static readonly RegisterOperand Xmm3 = new(Register.Xmm3, 8);
    public static readonly RegisterOperand Xmm4 = new(Register.Xmm4, 8);
    public static readonly RegisterOperand Xmm5 = new(Register.Xmm5, 8);
}
public record MemoryOperand : Operand
{
    private MemoryOperand(int size) : base(size)
    {

    }

    public static MemoryOperand FromRegister(Register baseReg, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = baseReg,
        };
    }
    public static MemoryOperand FromIndex(Register baseReg, Register indexReg, int scale, int displacement, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = baseReg,
            IndexRegister = indexReg,
            Scale = scale,
            Displacement = displacement
        };
    }
    public static MemoryOperand FromDisplacement(Register baseReg, int displacement, int size)
    {
        return new MemoryOperand(size)
        {
            BaseRegister = baseReg,
            Displacement = displacement,
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
    public Register? BaseRegister { get; init; }
    public Register? IndexRegister { get; init; }
    public int Scale { get; init; }
    public int Displacement { get; init; }

    public override string AsmName => GetName();

    private string GetName()
    {
        if (IsRel) return $"[rel {Label}]";

        var scalePart = Scale != 1 ? $"*{Scale}" : "";
        var indexPart = IndexRegister != null ? $"+{IndexRegister.Name}{scalePart}" : "";
        var offsetPart = Displacement != 0 ? $"{(Displacement > 0 ? $"+{Displacement}" : Displacement)}" : "";

        return $"[{BaseRegister!.Name}{indexPart}{offsetPart}]";
    }

    public MemoryOperand WithOffset(int offset, int newSize)
    {
        return this with
        {
            Displacement = Displacement + offset,
            Size = newSize,
        };
    }
}
public record ImmediateOperand(int Value, int Size) : Operand(Size)
{
    public override string AsmName => $"{Value}";

    public static readonly ImmediateOperand Zero = new(0, 8);
}
