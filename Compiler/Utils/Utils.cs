using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using xlang.Compiler.SemanticAnalysis;
using xlang.Compiler.Structures.AST;

namespace xlang.Compiler.Utils;

public static class Utils
{
    public static string GetFullFunctionName(string scope, string name) => $"{scope}.{name}";

    public static string SanitizeFqn(string fqn)
    {
        return fqn
            .Replace('(', '?')
            .Replace(')', '?')
            .Replace('[', '?')
            .Replace(']', '?')
            .Replace(',', '?')
            .Replace('*', '@');
    }

    private static int _nextId = 0;
    private static int GetNextId() => _nextId++;
}
