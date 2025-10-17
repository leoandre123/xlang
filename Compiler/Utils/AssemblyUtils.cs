using xlang.Compiler.CodeGeneration;

namespace xlang.Compiler.Utils;

public static class AssemblyUtils
{
    public static string GetSizeModifier(int size) => size switch { 1 => "byte", 2 => "word", 4 => "dword", 8 => "qword", _ => throw new NotSupportedException($"Incorrect size '{size}'") };
    public static string GetRegister(string name, int size)
    {
        return (name, size) switch
        {
            ("rax", 8) => "rax",
            ("rax", 4) => "eax",
            ("rax", 2) => "ax",
            ("rax", 1) => "al",
            ("rbx", 8) => "rbx",
            ("rbx", 4) => "ebx",
            ("rbx", 2) => "bx",
            ("rbx", 1) => "bl",
            ("rcx", 8) => "rcx",
            ("rcx", 4) => "ecx",
            ("rcx", 2) => "cx",
            ("rcx", 1) => "cl",
            ("rdx", 8) => "rdx",
            ("rdx", 4) => "edx",
            ("rdx", 2) => "dx",
            ("rdx", 1) => "dl",
            ("rsi", 8) => "rsi",
            ("rsi", 4) => "esi",
            ("rsi", 2) => "si",
            ("rsi", 1) => "sil",
            ("rdi", 8) => "rdi",
            ("rdi", 4) => "edi",
            ("rdi", 2) => "di",
            ("rdi", 1) => "dil",
            ("rbp", 8) => "rbp",
            ("rbp", 4) => "ebp",
            ("rbp", 2) => "bp",
            ("rbp", 1) => "bpl",
            ("rsp", 8) => "rsp",
            ("rsp", 4) => "esp",
            ("rsp", 2) => "sp",
            ("rsp", 1) => "spl",
            ("r8", 8) => "r8",
            ("r8", 4) => "r8d",
            ("r8", 2) => "r8w",
            ("r8", 1) => "r8b",
            ("r9", 8) => "r9",
            ("r9", 4) => "r9d",
            ("r9", 2) => "r9w",
            ("r9", 1) => "r9b",
            ("r10", 8) => "r10",
            ("r10", 4) => "r10d",
            ("r10", 2) => "r10w",
            ("r10", 1) => "r10b",
            ("r11", 8) => "r11",
            ("r11", 4) => "r11d",
            ("r11", 2) => "r11w",
            ("r11", 1) => "r11b",
            ("r12", 8) => "r12",
            ("r12", 4) => "r12d",
            ("r12", 2) => "r12w",
            ("r12", 1) => "r12b",
            ("r13", 8) => "r13",
            ("r13", 4) => "r13d",
            ("r13", 2) => "r13w",
            ("r13", 1) => "r13b",
            ("r14", 8) => "r14",
            ("r14", 4) => "r14d",
            ("r14", 2) => "r14w",
            ("r14", 1) => "r14b",
            ("r15", 8) => "r15",
            ("r15", 4) => "r15d",
            ("r15", 2) => "r15w",
            ("r15", 1) => "r15b",

            (
                "xmm0" or
                "xmm1" or
                "xmm2" or
                "xmm3" or
                "xmm4" or
                "xmm5", _) => name,



            (_, _) => throw new NotSupportedException($"There is no register '{name}' with size '{size}'")
        };
    }
    public static bool IsRegister(string name)
    {
        List<string> registers = ["rax",
        "rbx",
        "rcx",
        "rdx",
        "rsi",
        "rdi",
        "rbp",
        "rsp",
        "r8",
        "r9",
        "r10",
        "r11",
        "r12",
        "r13",
        "r14",
        "r15"];


        return registers.Contains(name);
    }
    public static bool FitsInRegister(int size) => size is 1 or 2 or 4 or 8;
    public static string GetStringConstLabel(int stringConstId) => $"str_{stringConstId}_";
    public static RegisterOperand GetParameterRegister(int index)
    {
        return index switch
        {
            0 => RegisterOperand.Rcx,
            1 => RegisterOperand.Rdx,
            2 => RegisterOperand.R8,
            3 => RegisterOperand.R9,
            _ => throw new NotSupportedException("Only the first 4 arguments are stored in registers")
        };
    }

    //LOCATIONS
    public static Operand GetParameterLocation(int index, bool isSpilled, int size = 8)
    {
        if (isSpilled || index >= 4)
            return new MemoryOperand($"[rbp + {16 + index * 8}]", size);
        return GetParameterRegister(index);
    }
}
