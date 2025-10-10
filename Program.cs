using System.CommandLine;
using System.Diagnostics;
using xlang.Compiler;

args = "--in program.xl console.xl".Split();

Console.WriteLine("""
                  ########################################################
                  #                  _                                   #
                  #                 | |                                  #
                  #            __  _| |     __ _ _ __   __ _             #
                  #            \ \/ / |    / _` | '_ \ / _` |            #
                  #             >  <| |___| (_| | | | | (_| |            #
                  #            /_/\_\_____/\__,_|_| |_|\__, |            #
                  #                                     __/ |            #
                  #                                    |___/             #
                  ########################################################
                  """);
Console.WriteLine("xLang compiler: v.0.0.0.1");
Console.WriteLine();

var inputOption = new Option<FileInfo[]>("--in")
{
    Description = "All source code files needed for the build",
    AllowMultipleArgumentsPerToken = true,
    Required = true
};

var outputOption = new Option<FileInfo>("--out")
{
    Description = "The path of the exe that will be built",
    DefaultValueFactory = _ => new FileInfo("Program.exe")
};

var rootCommand = new RootCommand("xlang")
{
    inputOption, outputOption
};



rootCommand.SetAction(parseResult =>
{
    var files = parseResult.GetValue(inputOption)!;
    var outFile = parseResult.GetValue(outputOption)!;

    if (!Compiler.Build(files.Select(x => x.FullName).ToArray(), new CompilerSettings
    {
        OutputName = outFile.FullName,
        Debug = true,
        PrintAst = false,
        PrintScopeTree = true,
        PrintAssembly = false
    }))
    {
        Console.ReadKey();
        return 1;
    }

    return 0;
});

var exitCode = rootCommand.Parse(args).Invoke();

if (exitCode == 0)
{
    Console.WriteLine("Running program:");
    var proc = Process.Start(new ProcessStartInfo(fileName: "program.exe"));

    proc.OutputDataReceived += (s, e) => { if (e.Data != null) Console.Out.WriteLine(e.Data); };
    proc.ErrorDataReceived += (s, e) => { if (e.Data != null) Console.Error.WriteLine(e.Data); };

    await proc.WaitForExitAsync();

    Console.WriteLine();
    Console.WriteLine($"Exit code: {proc.ExitCode}");
    Console.ReadKey();
}


return exitCode;