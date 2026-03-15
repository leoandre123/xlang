namespace xlang.Compiler.CodeGeneration;

public record Register
{
    public string Name { get; init; }
    public bool CalleeSaved { get; init; }
    public bool IsXmm { get; init; }
    public bool IsGpr => !IsXmm;

    private Register(string name, bool calleeSaved)
    {
        Name = name;
        CalleeSaved = calleeSaved;
    }

    private Register(string name, bool calleeSaved, bool isXmm)
    {
        Name = name;
        CalleeSaved = calleeSaved;
        IsXmm = isXmm;
    }


    public static readonly Register Rax = new("rax", false);
    public static readonly Register Rcx = new("rcx", false);
    public static readonly Register Rdx = new("rdx", false);
    public static readonly Register R8 = new("r8", false);
    public static readonly Register R9 = new("r9", false);
    public static readonly Register R10 = new("r10", false);
    public static readonly Register R11 = new("r11", false);

    public static readonly Register R12 = new("r12", true);
    public static readonly Register R13 = new("r13", true);
    public static readonly Register R14 = new("r14", true);
    public static readonly Register R15 = new("r15", true);

    public static readonly Register Rbp = new("rbp", true);
    public static readonly Register Rsp = new("rsp", true);

    public static readonly Register Xmm0 = new("xmm0", false, true);
    public static readonly Register Xmm1 = new("xmm1", false, true);
    public static readonly Register Xmm2 = new("xmm2", false, true);
    public static readonly Register Xmm3 = new("xmm3", false, true);
    public static readonly Register Xmm4 = new("xmm4", false, true);
    public static readonly Register Xmm5 = new("xmm5", false, true);
}
