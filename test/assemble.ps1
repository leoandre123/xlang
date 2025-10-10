nasm -f win64 -g -F cv8 program.asm -o program.obj -l program.lst
nasm -f win64 -g -F cv8 xlang_runtime.asm -o xlang_runtime.obj
link /nologo /debug /opt:noref /opt:noicf /incremental:no /subsystem:console /entry:main /out:program.exe program.obj xlang_runtime.obj kernel32.lib
.\program.exe

echo ""
echo "Exit code: $LASTEXITCODE"
