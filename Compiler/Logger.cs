using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace xlang.Compiler;

public enum LogLevel
{
    Trace,
    Debug,
    Info,
    Warning,
    Error,
    Critical
}
public static class Logger
{
    public static LogLevel CurrentLogLevel { get; set; } = LogLevel.Info;
    public static void Log(object msg, LogLevel level)
    {
        if (level < CurrentLogLevel) return;

        Console.ForegroundColor = level switch
        {
            LogLevel.Trace => ConsoleColor.Green,
            LogLevel.Debug => ConsoleColor.Gray,
            LogLevel.Info => ConsoleColor.White,
            LogLevel.Warning => ConsoleColor.Yellow,
            LogLevel.Error => ConsoleColor.Red,
            LogLevel.Critical => ConsoleColor.DarkRed,
            _ => throw new ArgumentOutOfRangeException(nameof(level), level, null)
        };
        Console.WriteLine($"{msg}");
    }

    public static void LogError(object msg) => Log(msg, LogLevel.Error);
    public static void LogInfo(object msg) => Log(msg, LogLevel.Info);
    public static void LogWarning(object msg) => Log(msg, LogLevel.Warning);

    public static void LogWarning(object msg, string filePath, SourceSpan span)
    {
        Log($"{filePath}({span.Start}, {span.End}) {msg}", LogLevel.Warning);
    }
    public static void LogDebug(object msg) => Log(msg, LogLevel.Debug);
    public static void LogTrace(object msg) => Log(msg, LogLevel.Trace);
}
