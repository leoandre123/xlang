using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace xlang.Compiler.CodeGeneration;

public record Register
{
    public string Name { get; init; }
    public bool CalleeSaved { get; init; }

    private Register(string name, bool calleeSaved)
    {
        Name = name;
        CalleeSaved = calleeSaved;
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
}
