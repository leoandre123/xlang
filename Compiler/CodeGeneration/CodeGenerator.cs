using System;
using System.ComponentModel.Design;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Drawing;
using System.Text;
using System.Threading;
using System.Xml.Linq;
using xlang.Compiler.Parsing;
using xlang.Compiler.SemanticAnalysis;
using xlang.Compiler.Structures;
using xlang.Compiler.Structures.AST;
using xlang.Compiler.Utils;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace xlang.Compiler.CodeGeneration;


public class AssemblyWriter
{
    private readonly StringBuilder _sb = new();
    private int _labelId;

    public void Append(string str, bool indented = true) => _sb.AppendLine(indented ? $"    {str}" : str);
    public void Comment(string comment)
    {
        foreach (var line in comment.Split(Environment.NewLine))
        {
            _sb.AppendLine($";  {line}");
        }
    }

    public void Label(string label) => _sb.AppendLine($"{label}:");
    public string CreateLabel(string label) => $"{label}_{_labelId++}";
    public override string ToString() => _sb.ToString();
    public void Insert(int pos, string str, bool indented = true) => _sb.Insert(pos, indented ? $"    {str}\n" : $"{str}\n");
    public int GetCurrentPos() => _sb.Length;
    public void Replace(string old, string replace) => _sb.Replace(old, replace);

}

enum EmitType
{
    Value,
    Address,
}
record EmitResult(EmitType Type, bool Xmm, int Size, string Reg)
{
    public static EmitResult Value(int size, bool isFloat = false) => new(EmitType.Value, isFloat, size, isFloat ? "xmm0" : "rax");
    public static EmitResult IntValue(int size, string reg = "rax") => new(EmitType.Value, false, size, reg);
    public static EmitResult FloatValue(int size, string reg = "xmm0") => new(EmitType.Value, true, size, reg);
    public static EmitResult Address(string reg = "rax") => new(EmitType.Address, false, 0, reg);
}

public class CodeGenerator
{
    private AssemblyWriter _writer = new();
    private bool _isMain = false;
    //private bool _rspAligned;
    private int _rspParity = 0;
    private string _currentBreakLabel;
    private int _localsSize;
    private int _tempCursor = 0;
    private int _tempMax = 0;
    private Dictionary<int, int> _largeParameterMap = new();


    private Module _module;
    private ProgramDeclaration _program;
    private SemanticModel _model;
    private string _source;

    private FunctionDeclaration _currentFunction;



    public CodeGenerator(Module module, SemanticModel semanticModel, string source)
    {
        _module = module;
        _program = module.Program;
        _model = semanticModel;
        _source = source;
    }

    public string Generate()
    {
        Logger.LogDebug($"Generating machine code {_program.Id}");

        List<string> externalFunction = ["ExitProcess", "GetProcessHeap", "HeapAlloc",
            .._model.ExternalFunctions[_module.Id]
                .Select(y => Utils.Utils.SanitizeFqn(_model.GetSymbol<CallableSymbol>(y).FullyQualifiedName))];
        var globals = _model.Symbols.Values.Where(x => x is FunctionSymbol { AccessType: EAccessType.Global } fs && x.OwnerModule.Program.Id == _program.Id).Select(y => Utils.Utils.SanitizeFqn((y as FunctionSymbol).FullyQualifiedName)).ToList();


        if (_model.GetEntryPoint()?.OwnerModule.Program.Id == _program.Id)
        {
            var label = Utils.Utils.SanitizeFqn(_model.GetEntryPoint()!.FullyQualifiedName);

            if (!globals.Contains(label))
            {
                globals.Add(label);
            }
        }

        _writer.Append("bits 64", false);
        _writer.Append("default rel", false);

        _writer.Append("section .rdata", false);

        for (var i = 0; i < _model.LiteralConstants.Count; i++)
        {
            _writer.Append(_model.LiteralConstants[i] switch
            {
                StringConstant s => $"LC{i}:\tdb \"{s.Text}\"".PadRight(50) + $"; {s.Text}",
                FloatConstant f => $"LC{i}:\tdd 0x{BitConverter.SingleToInt32Bits(f.Value):X8}".PadRight(50) + $"; {f.Value}f",
                _ => throw new ArgumentOutOfRangeException()
            }, false);
        }

        _writer.Append("section .text", false);
        if (externalFunction.Count != 0)
            _writer.Append($"extern {string.Join(", ", externalFunction)}");
        if (globals.Count != 0)
            _writer.Append($"global {string.Join(", ", globals)}");
        _writer.Append("");

        GenerateScope(_program.Body);

        return _writer.ToString();
    }

    public void GenerateScope(ScopeBody scope)
    {
        foreach (var s in scope.Scopes)
        {
            GenerateScope(s.Body);
        }
        foreach (var c in scope.Classes)
        {
            GenerateClass(c);
        }

        foreach (var f in scope.Functions)
        {
            if (f.IsExtern)
                continue;

            _currentFunction = f;
            GenerateFunction(f);
        }

    }

    public void GenerateClass(ClassDeclaration classDeclaration)
    {
        foreach (var member in classDeclaration.Members)
        {
            if (member is FunctionDeclaration methodDeclaration)
            {
                GenerateFunction(methodDeclaration);
            }
        }
    }

    public void GenerateFunction(FunctionDeclaration function)
    {
        var functionSymbol = _model.GetSymbol<FunctionSymbol>(function.Id);
        var meta = _model.FunctionData[functionSymbol.Id];
        _isMain = _model.EntryPoint == functionSymbol.Id;
        _rspParity = 0;
        _tempCursor = 0;
        _tempMax = 0;
        _localsSize = meta.LocalsSize;



        _writer.Label(Utils.Utils.SanitizeFqn(functionSymbol.FullyQualifiedName));

        EmitFunctionPrologue(function);
        EmitStatement(function.Body);

        if (function.ReturnType == "void")
        {
            if (_isMain)
            {
                Move(RegisterOperand.Rcx, ImmediateOperand.Zero);
                EmitFunctionCall("ExitProcess", 0);
            }
            else
            {
                Move(RegisterOperand.Rsp, RegisterOperand.Rbp);
                Pop(RegisterOperand.Rbp);
                _writer.Append("ret");
            }
        }

        var frameSize = meta.LocalsSize + _tempMax;
        _writer.Replace("___framesize___", $"{frameSize}");

        //EmitFunctionEpilogue(meta.LocalsSize);
    }

    public void EmitFunctionPrologue(FunctionDeclaration function)
    {
        var functionSymbol = _model.GetSymbol<FunctionSymbol>(function.Id);
        var meta = _model.FunctionData[functionSymbol.Id];

        _writer.Comment($"Locals: {meta.LocalsSize} bytes");
        _writer.Comment($"Leaf: {meta.IsLeaf}");
        _writer.Comment("Prologue");

        // if (!meta.IsLeaf)
        Push(RegisterOperand.Rbp);
        _rspParity = 0;

        // if (!meta.IsLeaf)
        Move(RegisterOperand.Rbp, RegisterOperand.Rsp);

        if (meta.LocalsSize != 0)
            _writer.Append("sub rsp, ___framesize___");

        for (var i = 0; i < Math.Min(4, function.Parameters.Count + (function.IsInstance ? 1 : 0)); i++)
        {
            Move(AssemblyUtils.GetParameterLocation(i, true), AssemblyUtils.GetParameterLocation(i, false));
        }

        _writer.Comment("Prologue End");
    }

    public void EmitStatement(Statement statement)
    {
        if (statement is (Return or Break or VariableDeclaration or Expression))
        {
            _writer.Comment(_source.Substring(statement.Span.Start, statement.Span.Length));
        }

        switch (statement)
        {
            case Block block:
                {
                    foreach (var s in block.Statements)
                        EmitStatement(s);
                    break;
                }
            case VariableDeclaration declaration:
                {
                    if (declaration.Initial != null)
                    {
                        var declType = _model.GetSymbol<VariableSymbol>(declaration.Id).Type;
                        if (declType is ClassTypeSymbol && declaration.Initial is CallExpression call && call.Name == declType.Name)
                        {
                            //Constructor call
                            var callableSymbol = _model.GetSymbol<CallableSymbol>(call.Id);

                            EmitFunctionCall(call.Name,
                                call.Arguments.Count,
                                () =>
                                {
                                    for (var i = 4; i < call.Arguments.Count; i++)
                                    {
                                        EmitExpression(call.Arguments[i]);
                                        var argType = _model.ExpressionData[call.Arguments[i].Id].Type;
                                        var parType = _model.GetSymbol<ParameterSymbol>(callableSymbol.Parameters[i]).Type;
                                        if (FitsInRegister(argType.Size))
                                        {
                                            //Move($"[rsp+{32 + (i - 4) * 8}]", "rax", parType.Size);
                                            Move(MemoryOperand.FromOffset("rsp", 32 + (i - 4) * 8, parType.Size), RegisterOperand.Rax);
                                        }
                                        else
                                        {
                                            MoveMemory(MemoryOperand.FromOffset("rsp", 32 + (i - 4) * 8, parType.Size), new MemoryOperand("[rax]", parType.Size), RegisterOperand.R11);
                                        }
                                    }


                                    if (call.Arguments.Count > 0) { EmitExpression(call.Arguments[0]); _writer.Append("mov rcx, rax"); }
                                    if (call.Arguments.Count > 1) { EmitExpression(call.Arguments[1]); _writer.Append("mov rdx, rax"); }
                                    if (call.Arguments.Count > 2) { EmitExpression(call.Arguments[2]); _writer.Append("mov r8, rax"); }
                                    if (call.Arguments.Count > 3) { EmitExpression(call.Arguments[3]); _writer.Append("mov r9, rax"); }



                                }
                            );
                        }
                        else if (declaration is not ArrayDeclaration)
                        {
                            EmitVariableAssignment(GetVariableDeclarationLocation(declaration), GetVariableDeclarationSize(declaration), declaration.Initial);
                        }
                        else
                        {
                            if (declaration.Initial is ArrayInitializerListExpression aile)
                            {
                                for (var i = 0; i < aile.Elements.Count; i++)
                                {
                                    EmitVariableAssignment(GetVariableDeclarationLocation(declaration, i), GetVariableDeclarationSize(declaration), aile.Elements[i]);
                                }
                            }
                        }
                    }
                    else
                    {
                        var count = declaration is ArrayDeclaration arr ? arr.Size : 1;
                        for (var i = 0; i < count; i++)
                            Move(new MemoryOperand(GetVariableDeclarationLocation(declaration, i), GetVariableDeclarationSize(declaration)), ImmediateOperand.Zero);
                    }

                    break;
                }
            case IfStatement ifStatement:
                {
                    var endIfLabel = _writer.CreateLabel("endif");
                    var elseLabel = ifStatement.Else == null ? "" : _writer.CreateLabel("else");

                    EmitExpression(ifStatement.Expression);
                    _writer.Append("test rax, rax");
                    _writer.Append($"jz {(ifStatement.Else == null ? endIfLabel : elseLabel)}");
                    EmitStatement(ifStatement.Body);

                    if (ifStatement.Else != null)
                    {
                        _writer.Append($"jmp {endIfLabel}");
                        _writer.Label(elseLabel);
                        EmitStatement(ifStatement.Else);
                    }

                    _writer.Label(endIfLabel);

                    break;
                }
            case WhileStatement whileStatement:
                {
                    var loopLabel = _writer.CreateLabel("loop");
                    var endWhileLabel = _writer.CreateLabel("endWhile");
                    _currentBreakLabel = endWhileLabel;

                    _writer.Label(loopLabel);
                    EmitExpression(whileStatement.Expression);
                    _writer.Append("test rax, rax");
                    _writer.Append($"jz {endWhileLabel}");
                    EmitStatement(whileStatement.Body);
                    _writer.Append($"jmp {loopLabel}");
                    _writer.Label(endWhileLabel);

                    break;
                }
            case ForStatement forStatement:
                {
                    var loopLabel = _writer.CreateLabel("loop");
                    var endForLabel = _writer.CreateLabel("endFor");
                    _currentBreakLabel = endForLabel;

                    EmitStatement(forStatement.Initial);
                    _writer.Label(loopLabel);
                    EmitExpression(forStatement.Expression);
                    _writer.Append("test rax, rax");
                    _writer.Append($"jz {endForLabel}");
                    EmitStatement(forStatement.Body);
                    EmitStatement(forStatement.Increment);
                    _writer.Append($"jmp {loopLabel}");
                    _writer.Label(endForLabel);
                    break;
                }
            case Break:
                _writer.Append($"jmp {_currentBreakLabel}");
                break;
            case Expression expression:
                EmitExpression(expression);
                break;
            case Return returnStatement:
                EmitReturn(returnStatement);
                break;
            default:
                {
                    throw new Exception($"Unknown statement type{statement.GetType()}");
                }
        }
    }

    private void EmitReturn(Return returnStatement)
    {
        if (returnStatement.Expression != null)
        {
            EmitExpression(returnStatement.Expression);
        }


        if (_isMain)
        {
            Move(RegisterOperand.Rcx, RegisterOperand.Rax);
            EmitFunctionCall("ExitProcess", 0);
        }
        else
        {
            Move(RegisterOperand.Rsp, RegisterOperand.Rbp);
            Pop(RegisterOperand.Rbp);
            _writer.Append("ret");
        }
    }

    /// <summary>
    /// Calculates <paramref name="expression"/> and stores the result in RAX.
    /// If the type of <paramref name="expression"/> is larger than 8 bytes the address of the result is stored in RAX
    /// </summary>
    /// <exception cref="NotSupportedException"></exception>
    private EmitResult EmitExpression(Expression expression, Operand? destination = null)
    {
        destination ??= Types.IsFloatType(_model.ExpressionData[expression.Id].Type) ?
            new RegisterOperand("xmm0", 8) :
            new RegisterOperand("rax", 8);

        if (_model.ConcatenationList.TryGetValue(expression.Id, out var concatList))
        {
            return EmitStringConcatenation(concatList);
        }

        switch (expression)
        {
            case IntegerLiteralExpression intLit:
                Move(destination, new ImmediateOperand(intLit.Value, 4));
                return EmitResult.Value(4);

            case FloatLiteralExpression floatLit:
                Move(destination, new MemoryOperand($"[rel LC{_model.LiteralConstantMap[floatLit.Id]}]", 4));
                //_writer.Append($"movss xmm0, [rel LC{_model.LiteralConstantMap[floatLit.Id]}]");
                return EmitResult.FloatValue(4);

            case BooleanLiteralExpression boolLit:
                Move(destination, new ImmediateOperand(boolLit.Value ? 1 : 0, 4));
                return EmitResult.Value(4);

            case StringLiteralExpression strLit:

                var strConstId = _model.LiteralConstantMap[strLit.Id];
                var strConst = _model.LiteralConstants[strConstId] as StringConstant;

                var strLbl = $"LC{strConstId}";
                var tmpVarOff = AllocateTemp(16);

                BinaryInstruction("lea", new RegisterOperand("rax", 8), new MemoryOperand($"[rel {strLbl}]", 8));
                Move(new MemoryOperand(GetLocation(tmpVarOff), 8), new RegisterOperand("rax", 8)); //data
                Move(new MemoryOperand(GetLocation(tmpVarOff, 8), 4), new ImmediateOperand(strConst.Text.Length, 4)); //length
                BinaryInstruction("lea", destination, new MemoryOperand($"{GetLocation(tmpVarOff)}", 8));

                return EmitResult.Address();
            case StringInterpolationExpression strIntExpr:
                return EmitStringInterpolation(strIntExpr);
            case VariableExpression varExpr:
                return EmitVariableExpression(varExpr, destination);
            case CallExpression callExpr:
                return EmitCallExpression(callExpr, destination);
            case UnaryExpression unaryExpr:
                return EmitUnary(unaryExpr, destination);
            case BinaryExpression binaryExpr:
                return EmitBinary(binaryExpr, destination);

            case IndexExpression indExpr:
                {
                    var baseType = _model.ExpressionData[indExpr.Base.Id].Type;
                    var elemType = baseType is PointerTypeSymbol p ? p.Type
                        : baseType;//(baseType as ArrayTypeSymbol)!.Type;

                    EmitIndexAddress(indExpr, destination);

                    if (FitsInRegister(elemType.Size))
                    {
                        Move(RegisterOperand.Rax, new MemoryOperand("[rax]", elemType.Size));
                        return EmitResult.Value(elemType.Size);
                    }
                    return EmitResult.Address();
                }
            case MemberAccessExpression accExpr:
                return EmitMemberAccess(accExpr, destination);

            case MemberCallExpression memCallExpr:
                return EmitMemberCallExpression(memCallExpr, destination);

            default:
                throw new NotSupportedException($"Expression not handled: {expression.GetType().Name}");
        }
    }


    /// <summary>
    /// Puts the address of <paramref name="expression"/> onto RAX
    /// </summary>
    /// <exception cref="NotSupportedException"></exception>
    private void EmitAddress(Expression expression, Operand destination)
    {
        switch (expression)
        {
            case VariableExpression varExpr:
                {
                    var loc = GetVariableAsOperand(varExpr);
                    if (IsLargeParameter(varExpr))
                        Move(destination, loc);
                    else
                    {
                        if (destination is RegisterOperand)
                            Lea(destination, loc);
                        else
                        {
                            Lea(RegisterOperand.Rax, loc);
                            Move(destination, RegisterOperand.Rax);
                        }
                    }

                    return;
                }
            case IndexExpression indExpr:
                {
                    EmitIndexAddress(indExpr, destination);
                    return;
                }
            case MemberAccessExpression memberAccess:
                //Klass members
                {
                    if (_model.ExpressionData[memberAccess.Base.Id].Type is ClassTypeSymbol cts)
                    {
                        var fieldSymbol = _model.GetSymbol<FieldSymbol>(memberAccess.Id);
                        if (memberAccess.Base is VariableExpression varExpr)
                        {
                            var loc = GetVariableAsOperand(varExpr);
                            if (IsLargeParameter(varExpr))
                                Move(destination, loc);
                            else
                                Lea(destination, loc);
                            return;
                        }
                    }

                    throw new NotSupportedException();
                    return;
                }

            default:
                throw new NotSupportedException($"Expression of type '{expression.GetType()}' is not addressable");
        }
    }
    /// <summary>
    /// Puts the address of <paramref name="indexExpression"/> onto RAX
    /// </summary>
    /// <exception cref="NotSupportedException"></exception>
    private void EmitIndexAddress(IndexExpression indexExpression, Operand destination)
    {
        //y[5] === *((&y)+5)
        //
        // int[] arr[]


        var baseType = _model.ExpressionData[indexExpression.Base.Id].Type;
        var elemType =
            baseType is PointerTypeSymbol p ? p.Type
            : baseType is ArrayTypeSymbol a ? a.Type
            : throw new NotSupportedException("Only array and pointer types can be indexed");

        if (baseType is PointerTypeSymbol)
            EmitExpression(indexExpression.Base, destination);
        else
            EmitAddress(indexExpression.Base, destination);

        _writer.Append("mov r10, rax");


        EmitExpression(indexExpression.Index);
        _writer.Append("mov r11, rax");

        _writer.Append($"imul r11, r11, {elemType.Size}");

        _writer.Append("add r10, r11");
        _writer.Append("mov rax, r10");
    }

    private EmitResult EmitCallExpression(CallExpression callExpression, Operand destination)
    {
        var funcSym = _model.GetSymbol<CallableSymbol>(callExpression.Id);

        EmitFunctionCall(Utils.Utils.SanitizeFqn(funcSym.FullyQualifiedName),
            callExpression.Arguments.Count,
            () =>
            {
                for (var i = 4; i < callExpression.Arguments.Count; i++)
                {
                    EmitExpression(callExpression.Arguments[i], new MemoryOperand($"[rsp + {32 + (i - 4) * 8}]", 8));
                }

                if (callExpression.Arguments.Count > 0) { EmitExpression(callExpression.Arguments[0], new RegisterOperand("rcx", 8)); }
                if (callExpression.Arguments.Count > 1) { EmitExpression(callExpression.Arguments[1], new RegisterOperand("rdx", 8)); }
                if (callExpression.Arguments.Count > 2) { EmitExpression(callExpression.Arguments[2], new RegisterOperand("r8", 8)); }
                if (callExpression.Arguments.Count > 3) { EmitExpression(callExpression.Arguments[3], new RegisterOperand("r9", 8)); }
            }
            );

        if (destination is not RegisterOperand { Name: "rax" })
        {
            Move(destination, new RegisterOperand("rax", 8));
        }

        return FitsInRegister(funcSym.Type.Size) ? EmitResult.Value(funcSym.Type.Size) : EmitResult.Address();
    }
    private EmitResult EmitMemberCallExpression(MemberCallExpression memCallExpr, Operand destination)
    {
        var methSym = _model.GetSymbol<CallableSymbol>(memCallExpr.Id);
        var callName = Utils.Utils.SanitizeFqn(methSym.FullyQualifiedName);
        var baseType = _model.ExpressionData[memCallExpr.Base.Id].Type;


        if (baseType is ScopeTypeSymbol)
        {
            EmitFunctionCall(callName, memCallExpr.Arguments.Select(x => (x, false)).ToList());
        }
        else
        {
            Debug.Assert(baseType is ClassTypeSymbol);
            EmitFunctionCall(callName, [(memCallExpr.Base, true), .. memCallExpr.Arguments.Select(x => (x, false))]);
        }


        if (destination is not RegisterOperand { Name: "rax" })
        {
            Move(destination, new RegisterOperand("rax", 8));
        }

        return FitsInRegister(methSym.Type.Size) ? EmitResult.Value(methSym.Type.Size) : EmitResult.Address();
    }

    private EmitResult EmitBinary(BinaryExpression binaryExpression, Operand destination)
    {
        if (binaryExpression.Operator.Type == OperatorType.Assign)
        {
            return EmitAssignment(binaryExpression, destination);
        }

        if (_model.ExpressionData[binaryExpression.Id].Type == Types.String)
        {
            return EmitStringOperation(binaryExpression);
        }

        if (binaryExpression.Operator.Type is
            OperatorType.AddAssign or
            OperatorType.SubAssign or
            OperatorType.MulAssign or
            OperatorType.DivAssign or
            OperatorType.ModAssign)
        {
            return EmitCompoundOperation(binaryExpression);
        }




        Operand left = destination;
        Operand right;
        var isFloat = Types.IsFloatType(_model.ExpressionData[binaryExpression.Id].Type);

        if (isFloat)
        {
            var rightResult = EmitExpression(binaryExpression.Right);
            Debug.Assert(rightResult.Type != EmitType.Address);

            if (!rightResult.Xmm)
            {
                IntToFloat(RegisterOperand.Xmm0, new RegisterOperand(rightResult.Reg, rightResult.Size));
            }
            Push(RegisterOperand.Xmm0);

            var leftResult = EmitExpression(binaryExpression.Left);

            Debug.Assert(leftResult.Type != EmitType.Address);
            if (!leftResult.Xmm)
            {
                IntToFloat(RegisterOperand.Xmm0, new RegisterOperand(leftResult.Reg, leftResult.Size));
            }

            Pop(RegisterOperand.Xmm1);

            //left = RegisterOperand.Xmm0 with { Size = leftResult.Size };
            left = left with { Size = leftResult.Size };
            right = RegisterOperand.Xmm1 with { Size = rightResult.Size };
        }
        else
        {
            if (binaryExpression.Right.IsNumericLiteral() && binaryExpression.Operator.Type is not (OperatorType.Div or OperatorType.Mod))
            {
                var leftResult = EmitExpression(binaryExpression.Left, destination);
                Debug.Assert(leftResult.Type != EmitType.Address);

                //left = RegisterOperand.Rax with { Size = leftResult.Size };
                left = left with { Size = leftResult.Size };
                right = new ImmediateOperand(binaryExpression.Right.GetNumericalLiteralValue(), 4);
            }
            else
            {
                var rightResult = EmitExpression(binaryExpression.Right);
                Debug.Assert(rightResult.Type != EmitType.Address);
                Push(RegisterOperand.Rax);
                var leftResult = EmitExpression(binaryExpression.Left);
                Debug.Assert(leftResult.Type != EmitType.Address);
                Pop(RegisterOperand.Rcx);
                //left = RegisterOperand.Rax with { Size = leftResult.Size };
                left = left with { Size = leftResult.Size };
                right = RegisterOperand.Rcx with { Size = rightResult.Size };
            }
        }

        //left = destination with { Size = left.Size };



        var maxSize = Math.Max(left.Size, right.Size);


        switch (binaryExpression.Operator.Type)
        {
            case OperatorType.Add:
                Add(left, right);
                return EmitResult.Value(maxSize, isFloat);

            case OperatorType.Sub:
                Sub(left, right);
                return EmitResult.Value(maxSize, isFloat);

            case OperatorType.Mul:
                Mul(left, right);
                return EmitResult.Value(maxSize, isFloat);

            case OperatorType.Div:
                Div(left, right);
                return EmitResult.Value(maxSize, isFloat);

            case OperatorType.Mod:
                Div(left, right);
                Move(left, RegisterOperand.Rdx);
                return EmitResult.Value(maxSize);

            case OperatorType.Equal:
                BinaryInstruction("cmp", left, right);
                _writer.Append("sete al");
                _writer.Append("movzx rax, al");
                return EmitResult.Value(1);


            case OperatorType.NotEqual:
                BinaryInstruction("cmp", left, right);
                _writer.Append("setne al");
                _writer.Append("movzx rax, al");
                return EmitResult.Value(1);

            case OperatorType.Less:
                BinaryInstruction("cmp", left, right);
                _writer.Append("setl al");
                _writer.Append("movzx rax, al");
                return EmitResult.Value(1);

            case OperatorType.LessOrEqual:
                BinaryInstruction("cmp", left, right);
                _writer.Append("setle al");
                _writer.Append("movzx rax, al");
                return EmitResult.Value(1);

            case OperatorType.Greater:
                BinaryInstruction("cmp", left, right);
                _writer.Append("setg al");
                _writer.Append("movzx rax, al");
                return EmitResult.Value(1);

            case OperatorType.GreaterOrEqual:
                BinaryInstruction("cmp", left, right);
                _writer.Append("setge al");
                _writer.Append("movzx rax, al");
                return EmitResult.Value(1);
            default:
                throw new NotSupportedException($"Binary {binaryExpression.Operator.Symbol}");
        }


    }
    private EmitResult EmitUnary(UnaryExpression unaryExpression, Operand destination)
    {
        switch (unaryExpression.Operator.Type)
        {
            case OperatorType.PreIncrement:
                EmitIncrementDecrement((unaryExpression.Operand as VariableExpression) ?? throw new Exception("Must be an l-value"), true, true);
                break;
            case OperatorType.PreDecrement:
                EmitIncrementDecrement((unaryExpression.Operand as VariableExpression) ?? throw new Exception("Must be an l-value"), false, true);
                break;
            case OperatorType.PostIncrement:
                EmitIncrementDecrement((unaryExpression.Operand as VariableExpression) ?? throw new Exception("Must be an l-value"), true, false);
                break;
            case OperatorType.PostDecrement:
                EmitIncrementDecrement((unaryExpression.Operand as VariableExpression) ?? throw new Exception("Must be an l-value"), false, false);
                break;
            case OperatorType.Neg:
                EmitExpression(unaryExpression.Operand, destination);
                _writer.Append($"neg {destination.AsmName}");
                break;
            case OperatorType.Address:
                {
                    var varExpr = (unaryExpression.Operand as VariableExpression) ?? throw new Exception("Tried to get the address of an r-value");
                    var loc = GetVariableExpressionLocation(varExpr);
                    //_writer.Append($"lea rax, {loc}");
                    BinaryInstruction("lea", destination, new MemoryOperand(loc, 8));
                    return EmitResult.Value(8);
                }
            case OperatorType.Dereference: // *a
                {
                    var type = (_model.ExpressionData[unaryExpression.Operand.Id].Type as PointerTypeSymbol).Type;
                    if (EmitExpression(unaryExpression.Operand, FitsInRegister(type.Size) ? new RegisterOperand("rax", 8) : destination).Type == EmitType.Value)
                    {
                        if (FitsInRegister(type.Size))
                        {
                            Move(destination, new MemoryOperand("[rax]", type.Size));
                            return EmitResult.Value(type.Size);
                        }
                        return EmitResult.Address();
                    }

                    throw new NotSupportedException("A pointer should be a value");
                }

            default: throw new NotSupportedException($"Unary not handled '{unaryExpression.Operator.Symbol}'");
        }

        return EmitResult.Value(_model.ExpressionData[unaryExpression.Operand.Id].Type.Size);
    }

    private EmitResult EmitStringOperation(BinaryExpression expression)
    {
        /*
         * Koncept
         *
         *
         */


        if (expression.Operator.Type == OperatorType.Add)
        {
            if (_model.ExpressionData[expression.Left.Id].Type == Types.String)
            {
                //Load string length;
            }
        }
        return EmitResult.Address();
    }
    private EmitResult EmitStringConcatenation(ConcatenationList concatenationList)
    {
        if (concatenationList.List.Count == 1)
            return EmitExpression(concatenationList.List[0]);

        var tmpVarOff = AllocateTemp(16);
        Move(new MemoryOperand(GetLocation(tmpVarOff, 8), 4), new ImmediateOperand(0, 4));

        //Calculate result size
        foreach (var expr in concatenationList.List)
        {
            var type = _model.ExpressionData[expr.Id].Type;
            EmitExpression(expr);

            if (type == Types.String)
            {
                Move(new RegisterOperand("rax", 4), new MemoryOperand("[rax+8]", 4));
                Add(new MemoryOperand(GetLocation(tmpVarOff, 8), 4), new MemoryOperand("rax", 4));
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        EmitHeapAllocation(new MemoryOperand(GetLocation(tmpVarOff, 8), 4));


        Move(new MemoryOperand(GetLocation(tmpVarOff), 8), new RegisterOperand("rax", 8)); //Place heap ptr in string struct


        Push(new RegisterOperand("rax", 8));
        foreach (var expr in concatenationList.List)
        {
            var type = _model.ExpressionData[expr.Id].Type;
            EmitExpression(expr);

            if (type == Types.String)
            {
                Pop(new RegisterOperand("r10", 8));

                Move(new RegisterOperand("r11", 8), new MemoryOperand("[rax]", 8));
                Move(new RegisterOperand("rcx", 4), new MemoryOperand("[rax+8]", 4));


                //rax: ptr to string (ptr*, len)*
                //r10: heap ptr
                //r11: str data ptr
                //rcx: bytes left


                var lblLoop = _writer.CreateLabel("str_copy");
                var lblEnd = _writer.CreateLabel("str_copy_end");

                _writer.Label(lblLoop);

                BinaryInstruction("test", new RegisterOperand("rcx", 4), new RegisterOperand("rcx", 4));
                _writer.Append($"jz {lblEnd}");

                Move(new RegisterOperand("rdx", 1), new MemoryOperand("[r11]", 1));
                Move(new MemoryOperand("[r10]", 1), new RegisterOperand("rdx", 1));



                _writer.Append($"inc {GetRegister("r11", 8)}"); //Increase form string pointer
                _writer.Append($"inc {GetRegister("r10", 8)}"); //Increase to string pointer
                _writer.Append($"dec {GetRegister("rcx", 4)}"); //Decrease bytes left
                _writer.Append($"jmp {lblLoop}");
                _writer.Label(lblEnd);



                Push(new RegisterOperand("r10", 8));
            }
            else
            {
                throw new NotImplementedException();
            }
        }


        Pop(new RegisterOperand("rax", 8));

        Lea(RegisterOperand.Rax, new MemoryOperand(GetLocation(tmpVarOff), 8));

        return EmitResult.Address();
    }
    private EmitResult EmitStringInterpolation(StringInterpolationExpression interpolation)
    {
        //foreach (var part in interpolation.Parts)
        //{
        //    part
        //}

        throw new NotImplementedException();
    }

    private EmitResult EmitCompoundOperation(BinaryExpression expression)
    {
        if (expression.Left is not VariableExpression varExpr)
            throw new NotSupportedException("Can only assign to lvalues");
        var varOp = GetVariableAsOperand(varExpr);

        var isLit = expression.Right.IsNumericLiteral();
        var litVal = isLit ? expression.Right.GetNumericalLiteralValue() : -1;

        var isPtr = false;
        var ptrTypeSize = 0;
        if (_model.ExpressionData[expression.Left.Id].Type is PointerTypeSymbol ptr)
        {
            isPtr = true;
            ptrTypeSize = ptr.Type.Size;
            litVal *= ptrTypeSize;
        }


        if (isLit && expression.Operator.Type is (OperatorType.AddAssign or OperatorType.SubAssign or OperatorType.MulAssign))
        {
            switch (expression.Operator.Type)
            {
                case OperatorType.AddAssign: Add(varOp, new ImmediateOperand(litVal, varOp.Size)); break;
                case OperatorType.SubAssign: Sub(varOp, new ImmediateOperand(litVal, varOp.Size)); break;
                case OperatorType.MulAssign: Mul(varOp, new ImmediateOperand(litVal, varOp.Size)); break;
            }
            Move(RegisterOperand.Rax, varOp);
        }
        else
        {
            EmitExpression(expression.Right);
            if (isPtr) Mul(RegisterOperand.Rax, new ImmediateOperand(ptrTypeSize, 4));
            Move(RegisterOperand.R10, RegisterOperand.Rax);
            Move(RegisterOperand.Rax, varOp);

            switch (expression.Operator.Type)
            {
                case OperatorType.AddAssign: Add(RegisterOperand.Rax, RegisterOperand.R10); break;
                case OperatorType.SubAssign: Sub(RegisterOperand.Rax, RegisterOperand.R10); break;
                case OperatorType.MulAssign: Mul(RegisterOperand.Rax, RegisterOperand.R10); break;
                case OperatorType.DivAssign: Div(RegisterOperand.Rax, RegisterOperand.R10); break;
                case OperatorType.ModAssign: Mod(RegisterOperand.Rax, RegisterOperand.R10); break;
            }

            Move(varOp, RegisterOperand.Rax);
        }

        return EmitResult.Value(varOp.Size);
    }
    private EmitResult EmitAssignment(BinaryExpression expression, Operand destination)
    {
        var left = _model.ExpressionData[expression.Left.Id].Type;
        var right = _model.ExpressionData[expression.Right.Id].Type;
        //var size = Math.Min(left.Size, right.Size);

        if (expression.Left is VariableExpression varExpr && FitsInRegister(left.Size))
        {
            if (Types.IsFloatType(left) && Types.IsIntegerType(right))
            {
                EmitExpression(expression.Right, RegisterOperand.Rax);
                IntToFloat(destination, RegisterOperand.Rax);
                Move(new MemoryOperand(GetVariableExpressionLocation(varExpr), left.Size), destination with { Size = left.Size });
            }
            else if (Types.IsIntegerType(left) && Types.IsFloatType(right))
            {
                var res = EmitExpression(expression.Right, RegisterOperand.Xmm0);
                FloatToInt(destination, RegisterOperand.Xmm0 with { Size = res.Size });
                Move(new MemoryOperand(GetVariableExpressionLocation(varExpr), left.Size), destination with { Size = left.Size });
            }
            else
            {
                EmitExpression(expression.Right, destination);
                Move(new MemoryOperand(GetVariableExpressionLocation(varExpr), left.Size), destination with { Size = left.Size });
            };

            return EmitResult.Value(left.Size);
        }
        else
        {
            EmitAddress(expression.Left, RegisterOperand.Rax);
            Push(RegisterOperand.Rax);

            if (EmitExpression(expression.Right, destination).Type == EmitType.Value)
            {
                Pop(RegisterOperand.Rcx);
                Move(new MemoryOperand("[rcx]", left.Size), destination);
                return EmitResult.Value(left.Size);
            }
            else
            {
                Pop(RegisterOperand.Rcx);
                Move(new MemoryOperand("[rcx]", left.Size), new MemoryOperand(destination.AsmName, right.Size));
                return EmitResult.Address();
            }
        }




        //if (expression.Left is VariableExpression varExpr && FitsInRegister(left.Size))
        //{
        //    if (EmitExpression(expression.Right).Type == EmitType.Value)
        //    {
        //        
        //        Move(new MemoryOperand(GetVariableExpressionLocation(varExpr), left.Size), new RegisterOperand("rax", right.Size));
        //        return EmitResult.Value(size);
        //    }
        //    else
        //    {
        //        Move("r11", "[rax]", size);
        //        Move(GetVariableExpressionLocation(varExpr), "r11", size);
        //        return EmitResult.Address();
        //    }
        //}

        //{
        //    EmitAddress(expression.Left, destination);
        //    Push(new RegisterOperand("rax", 8));
        //    if (EmitExpression(expression.Right).Type == EmitType.Value) //Value -> address
        //    {
        //        Pop(new RegisterOperand("r10", 8));
        //        Move("[r10]", "rax", size);
        //        return EmitResult.Value(size);
        //    }
        //    else //Address -> address
        //    {
        //        Pop(new RegisterOperand("r10", 8));
        //        var off = 0;
        //        while (off < size)
        //        {
        //            var sizeToMove = Math.Min(8, size - off);
        //
        //            Move("r11", $"[rax+{off}]", sizeToMove);
        //            Move($"[r10+{off}]", "r11", sizeToMove);
        //
        //            off += sizeToMove;
        //        }
        //        return EmitResult.Address();
        //    }
        //}

    }

    private EmitResult EmitVariableExpression(VariableExpression variableExpression, Operand destination)
    {
        var sym = _model.GetSymbol(variableExpression.Id) as SymbolWithType;
        var size = sym.Type.Size;

        if (sym is FieldSymbol fieldSymbol)
        {
            var thisPtr = AssemblyUtils.GetParameterLocation(0, true);
            Move(RegisterOperand.Rax, thisPtr);
            if (FitsInRegister(size))
            {
                var regSize = Math.Max(4, size);
                Move(destination, MemoryOperand.FromOffset("rax", fieldSymbol.Offset, size));
                return IsFloatOperand(destination) ? EmitResult.FloatValue(regSize) : EmitResult.Value(regSize);
            }
            Lea(destination, MemoryOperand.FromOffset("rax", fieldSymbol.Offset, size));
            return EmitResult.Address();
        }
        else
        {
            var loc = GetVariableExpressionLocation(variableExpression);
            if (FitsInRegister(size))
            {
                var regSize = Math.Max(4, size);
                Move(destination with { Size = regSize }, new MemoryOperand(loc, size));
                return IsFloatOperand(destination) ? EmitResult.FloatValue(regSize) : EmitResult.Value(regSize);
            }

            EmitAddress(variableExpression, destination);
            return EmitResult.Address();
        }
    }
    private EmitResult EmitMemberAccess(MemberAccessExpression memberAccess, Operand destination)
    {
        var baseType = _model.ExpressionData[memberAccess.Base.Id].Type;

        if (baseType is ClassTypeSymbol classTypeSymbol)
        {
            var fieldSymbol = _model.GetSymbol<FieldSymbol>(memberAccess.Id);
            if (memberAccess.Base is VariableExpression varExpr)
            {
                var loc = GetVariableExpressionLocation(varExpr, byteOffset: classTypeSymbol.FieldOffset[fieldSymbol.Id]);
                var size = fieldSymbol.Type.Size;

                if (FitsInRegister(size))
                {
                    var regSize = Math.Max(4, size);
                    Move(destination with { Size = regSize }, new MemoryOperand(loc, size));
                    return IsFloatOperand(destination) ? EmitResult.FloatValue(regSize) : EmitResult.Value(regSize);
                }

                EmitAddress(varExpr, destination);
                return EmitResult.Address();
            }
        }
        else if (baseType is PointerTypeSymbol pointerSymbol)
        {
            if (pointerSymbol.Type is ClassTypeSymbol classTypeSymbolPtr)
            {
                var fieldSymbol = _model.GetSymbol<FieldSymbol>(memberAccess.Id);
                if (memberAccess.Base is VariableExpression varExpr)
                {
                    var ptrOp = GetVariableAsOperand(varExpr);

                    Move(RegisterOperand.Rax, ptrOp);
                    if (FitsInRegister(fieldSymbol.Type.Size))
                    {
                        var regSize = Math.Max(4, fieldSymbol.Type.Size);
                        Move(destination with { Size = regSize }, MemoryOperand.FromOffset("rax", classTypeSymbolPtr.FieldOffset[fieldSymbol.Id], regSize));
                        return IsFloatOperand(destination) ? EmitResult.FloatValue(regSize) : EmitResult.Value(regSize);
                    }
                    Lea(destination, MemoryOperand.FromOffset("rax", fieldSymbol.Offset, 8));
                    return EmitResult.Address();
                }


            }
        }
        else if (baseType == Types.String)
        {
            if (memberAccess.Member == "length")
            {
                EmitAddress(memberAccess.Base, RegisterOperand.Rax);
                Move(destination with{Size = 4}, MemoryOperand.FromOffset("rax", 8, 4));
                return EmitResult.Value(4);
            }
            if (memberAccess.Member == "data")
            {
                EmitAddress(memberAccess.Base, RegisterOperand.Rax);
                Move(destination, new MemoryOperand("[rax]", 8));
                return EmitResult.Value(8);
            }
        }

        throw new NotSupportedException($"Expression not handled: '{memberAccess.Member}' on '{baseType}'"); ;
    }



    private void EmitHeapAllocation(Operand lengthOperand)
    {
        EmitFunctionCall("GetProcessHeap", 0);

        Move(new RegisterOperand("r11", 4), lengthOperand);

        EmitFunctionCall("HeapAlloc", 0, () =>
        {
            _writer.Append("mov rcx, rax");
            _writer.Append("xor edx, edx");
            _writer.Append("mov r8d, r11d");
        });
    }


    private void EmitFunctionCall(string name, List<(Expression exp, bool address)> arguments)
    {
        EmitFunctionCall(name,
            arguments.Count,
            () =>
            {

                for (var i = 0; i < arguments.Count; i++)
                {
                    var argType = _model.ExpressionData[arguments[i].exp.Id].Type;

                    if (FitsInRegister(argType.Size) && !arguments[i].address)
                    {
                        EmitExpression(arguments[i].exp, AssemblyUtils.GetParameterLocation(i, false));
                    }
                    else
                    {
                        EmitAddress(arguments[i].exp, AssemblyUtils.GetParameterLocation(i, false));
                    }

                }
            }
        );
    }
    private void EmitFunctionCall(string name, int argCount, Action? preCall = null)
    {
        var reserve = Math.Max(argCount * 8, 32);

        if (_rspParity % 16 != 0) reserve += (16 - _rspParity % 16);

        SubRsp(reserve);

        preCall?.Invoke();

        _writer.Append($"call {name}");
        AddRsp(reserve);
    }

    private void EmitVariableAssignment(string location, int size, Expression expression)
    {
        if (expression.IsNumericLiteral())
        {
            Move(new MemoryOperand(location, size), new ImmediateOperand(expression.GetNumericalLiteralValue(), size));
        }
        else if (expression is StringLiteralExpression strLit)
        {
            var strLbl = $"LC{_model.LiteralConstantMap[strLit.Id]}";
            Lea(RegisterOperand.Rax, MemoryOperand.FromLabel(strLbl, 8));
            Move(new MemoryOperand(location, 8), new RegisterOperand("rax", 8));
            Move(new MemoryOperand(location, 8).WithOffset(8, 4), new ImmediateOperand(strLit.Value.Length, 4));
        }
        else
        {
            var exprType = _model.ExpressionData[expression.Id].Type;
            if (FitsInRegister(exprType.Size))
            {
                var emitResult = EmitExpression(expression, new MemoryOperand(location, size));
                Debug.Assert(emitResult.Type == EmitType.Value);
            }
            else
            {
                var emitResult = EmitExpression(expression);
                Debug.Assert(emitResult.Type == EmitType.Address);
                Move(new MemoryOperand(location, 8), new MemoryOperand("[rax]", 8));
            }
        }
    }

    //Unaries
    private void EmitIncrementDecrement(VariableExpression operand, bool increment, bool prefix)
    {
        var loc = GetVariableExpressionLocation(operand);
        var size = GetVariableExpressionSize(operand);
        var sizeModifier = GetSizeModifier(size);

        EmitExpression(operand);

        _writer.Append($"{(increment ? "inc" : "dec")} {sizeModifier} {loc}");
        if (prefix)
        {
            _writer.Append($"{(increment ? "inc" : "dec")} {sizeModifier} {GetRegister("rax", size)}");
        }
    }



    //Helpers (Do not write to assembly)

    private string GetLocation(int location, int byteOffset = 0) => $"[rbp-{location - byteOffset}]";
    private string GetParameterRegister(int index)
    {
        return index switch
        {
            0 => "rcx",
            1 => "rdx",
            2 => "r8",
            3 => "r9",
            _ => throw new NotSupportedException("Only the first 4 arguments are stored in registers")
        };
    }
    private int GetVariableExpressionSize(IndexExpression indexExpression)
    {
        return _model.GetSymbol<VariableSymbol>(indexExpression.Id).Type.Size;
    }
    private int GetVariableExpressionSize(VariableExpression variableExpression)
    {
        return _model.GetSymbol<SymbolWithType>(variableExpression.Id).Type.Size;
    }
    private string GetVariableExpressionLocation(VariableExpression variableExpression, int offset = 0, int byteOffset = 0)
    {
        var sym = _model.GetSymbol(variableExpression.Id);
        if (sym is VariableSymbol varSym)
            return GetVariableLocation(varSym, offset, byteOffset);
        if (sym is ParameterSymbol parSym)
            return GetParameterLocation(parSym, offset, byteOffset);
        //if (sym is FieldSymbol fieldSym)
        //    return GetParameterLocation(parSym, offset, byteOffset);
        throw new Exception("Cannot find variable");
    }


    private string GetParameterLocation(ParameterSymbol parSym, int offset = 0, int byteOffset = 0)
    {
        return $"[rbp + {16 + parSym.Index * 8 + byteOffset}]";
    }
    private string GetVariableLocation(VariableSymbol varSym, int offset = 0, int byteOffset = 0) => $"[rbp-{varSym.Offset - offset * varSym.Type.Size - byteOffset}]";

    private bool IsLargeParameter(VariableExpression variableExpression)
    {
        if (_model.ExpressionData[variableExpression.Id].Type.Size <= 8)
            return false;

        //if (_model.ParameterSymbols.ContainsKey(variableExpression.Id))
        //{
        //    return true;
        //}
        //TODO: FIX

        return false;
    }

    private string GetVariableDeclarationLocation(VariableDeclaration declaration, int offset = 0, int byteOffset = 0)
    {
        return GetVariableLocation(_model.GetSymbol<VariableSymbol>(declaration.Id), offset, byteOffset);
    }
    private int GetVariableDeclarationSize(VariableDeclaration declaration)
    {
        return _model.GetSymbol<VariableSymbol>(declaration.Id).Type.Size;
    }
    private string GetSizeModifier(int size) => size switch { 1 => "byte", 2 => "word", 4 => "dword", 8 => "qword", _ => throw new NotSupportedException($"Incorrect size '{size}'") };
    private static string GetRegister(string name, int size)
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
    private bool IsRegister(string name)
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
    private bool FitsInRegister(int size) => size is 1 or 2 or 4 or 8;
    private string GetStringConstLabel(int stringConstId) => $"str_{stringConstId}_";
    private bool IsFloatOperation(Operand dest, Operand src)
    {
        return (dest is RegisterOperand dr && dr.Name.StartsWith("xmm")) ||
               (src is RegisterOperand sr && sr.Name.StartsWith("xmm"));
    }

    private bool IsFloatOperand(Operand op) => op is RegisterOperand reg && reg.Name.StartsWith("xmm");

    private Operand GetVariableAsOperand(VariableExpression variableExpression)
    {
        var loc = GetVariableExpressionLocation(variableExpression);
        var size = GetVariableExpressionSize(variableExpression);
        return new MemoryOperand(loc, size);
    }


    //Assembly instructions (USE ONLY THESE)

    private void Move(Operand dest, Operand src, bool signed = false)
    {
        if (IsFloatOperation(dest, src))
        {
            MoveFloat(dest, src);
            return;
        }
        if (dest is MemoryOperand mDest && src is MemoryOperand sDest)
        {
            //throw new NotSupportedException("Both operands cannot be memory");
            MoveMemory(mDest, sDest, new RegisterOperand("rdx", 8));
            return;
        }
        if (dest is ImmediateOperand)
            throw new NotSupportedException("Immediate cannot be used as destination");

        if (dest is MemoryOperand && src is ImmediateOperand)
        {
            var destSizeModifier = GetSizeModifier(dest.Size);
            _writer.Append($"mov {destSizeModifier} {dest.AsmName}, {src.AsmName}");
        }
        else if (src.Size < dest.Size)
        {
            if (dest is MemoryOperand)
            {
                var tmp = new RegisterOperand("r15", dest.Size);
                Move(tmp, src);
                Move(dest, tmp);
            }
            else if (signed)
            {
                var srcSizeModifier = GetSizeModifier(src.Size);
                _writer.Append($"movsx {dest.AsmName}, {srcSizeModifier} {src.AsmName}");
            }
            else if (src.Size == 4)
            {
                _writer.Append($"mov {dest.AsmName}, {src.AsmName}");
            }
            else
            {
                var srcSizeModifier = GetSizeModifier(src.Size);
                _writer.Append($"movzx {dest.AsmName}, {srcSizeModifier} {src.AsmName}");
            }

        }
        else if (dest.Size < src.Size)
        {
            if (dest is MemoryOperand)
            {
                _writer.Append($"mov {dest.AsmName}, {((src as RegisterOperand) with { Size = dest.Size }).AsmName}");
            }
            else
            {
                _writer.Append($"mov {dest.AsmName}, {src.AsmName}");
            }

        }
        else
        {
            _writer.Append($"mov {dest.AsmName}, {src.AsmName}");
        }

    }
    private void MoveMemory(MemoryOperand dest, MemoryOperand src, RegisterOperand temporaryRegister)
    {
        if (dest.Size != src.Size)
        {
            throw new NotSupportedException($"The sizes of the operands does not match");
        }


        var bytesLeft = dest.Size;
        var offset = 0;

        while (bytesLeft > 0)
        {
            var moveSize = Math.Min(bytesLeft, temporaryRegister.Size);
            if (moveSize is not (1 or 2 or 4 or 8))
                throw new NotSupportedException();

            var from = src.WithOffset(offset, moveSize);
            var to = dest.WithOffset(offset, moveSize);
            var tmp = temporaryRegister with { Size = moveSize };

            Move(tmp, from);
            Move(to, tmp);

            offset += moveSize;
            bytesLeft -= moveSize;
        }

    }
    private void MoveFloat(Operand dest, Operand src)
    {
        Debug.Assert(IsFloatOperation(dest, src));
        Debug.Assert(src is not ImmediateOperand);
        Debug.Assert(dest is not ImmediateOperand);
        Debug.Assert(src.Size == dest.Size);

        if (dest.Size == 4)
            _writer.Append($"movss {dest.AsmName}, {src.AsmName}");
        else if (dest.Size == 8)
            _writer.Append($"movsd {dest.AsmName}, {src.AsmName}");
    }

    private void BinaryInstruction(string op, Operand dest, Operand src)
    {
        if (dest is ImmediateOperand)
            throw new NotSupportedException($"Destination cannot be an immediate '{dest}'");

        if (dest is MemoryOperand && src is MemoryOperand)
            throw new NotSupportedException("Both operands cannot be memory");

        if (src.Size != dest.Size)
            throw new NotSupportedException("Operands have different sizes");

        if (src is ImmediateOperand && dest is MemoryOperand)
        {
            var sizeModifier = GetSizeModifier(dest.Size);
            _writer.Append($"{op} {sizeModifier} {dest.AsmName}, {src.AsmName}");
        }
        else
        {
            _writer.Append($"{op} {dest.AsmName}, {src.AsmName}");
        }
    }
    private void IntegerOrFloatBinaryInstruction(string normalOp, string floatOp, string doubleOp, Operand dest, Operand src)
    {

        if (IsFloatOperation(dest, src))
        {
            if (!IsFloatOperand(dest) || src is ImmediateOperand)
                throw new NotSupportedException();

            if (dest.Size == 4)
                BinaryInstruction(floatOp, dest, src);
            else if (dest.Size == 8)
                BinaryInstruction(doubleOp, dest, src);
        }
        else
        {
            BinaryInstruction(normalOp, dest, src);
        }
    }


    private void Lea(Operand dest, Operand src) => BinaryInstruction("lea", dest, src with { Size = 8 });


    private void Add(Operand dest, Operand src) => IntegerOrFloatBinaryInstruction("add", "addss", "addsd", dest, src);
    private void Sub(Operand dest, Operand src) => IntegerOrFloatBinaryInstruction("sub", "subss", "subsd", dest, src);
    private void Mul(Operand dest, Operand src) => IntegerOrFloatBinaryInstruction("imul", "mulss", "mulsd", dest, src);
    private void Div(Operand dest, Operand src)
    {
        Debug.Assert(dest.Size == src.Size);
        if (IsFloatOperation(dest, src))
        {
            if (!IsFloatOperand(dest) || src is ImmediateOperand)
                throw new NotSupportedException();

            if (dest.Size == 4)
                BinaryInstruction("divss", dest, src);
            else if (dest.Size == 8)
                BinaryInstruction("divsd", dest, src);
        }
        else
        {
            if (dest is RegisterOperand { Name: "rax" })
            {
                _writer.Append("cqo");
                _writer.Append($"idiv {src.AsmName}");
            }
            else
            {
                Move(RegisterOperand.Rax, dest);
                _writer.Append("cqo");
                _writer.Append($"idiv {src.AsmName}");
                Move(dest, RegisterOperand.Rax);
            }
        }
    }
    private void Mod(Operand dest, Operand src) => throw new NotImplementedException();

    private void ExtendInt(Operand dest, Operand src, bool signed)
    {
        Debug.Assert(src.Size is 1 or 2);
        Debug.Assert(dest.Size is 2 or 4 or 8);
        Debug.Assert(dest is RegisterOperand);
        Debug.Assert(src is not ImmediateOperand);
        _writer.Append($"{(signed ? "movsx" : "movzx")} {dest}, {src}");
    }
    private void IntToFloat(Operand dest, Operand src)
    {
        Debug.Assert(dest is not ImmediateOperand);
        Debug.Assert(src is not ImmediateOperand);
        Debug.Assert(IsFloatOperand(dest));

        if (dest.Size == 4)
            _writer.Append($"cvtsi2ss {dest.AsmName}, {src.AsmName}");
        else if (dest.Size == 8)
            _writer.Append($"cvtsi2sd {dest.AsmName}, {src.AsmName}");
    }
    private void FloatToInt(Operand dest, Operand src)
    {
        Debug.Assert(dest is RegisterOperand { IsGpr: true });
        Debug.Assert(src is RegisterOperand { IsXmm: true });

        if (src.Size == 4)
            _writer.Append($"cvtss2si {dest.AsmName}, {src.AsmName}");
        else if (src.Size == 8)
            _writer.Append($"cvtsd2si {dest.AsmName}, {src.AsmName}");
    }
    private void Push(Operand operand)
    {
        if (IsFloatOperand(operand))
        {
            SubRsp(16);
            _writer.Append($"movdqu [rsp], {operand.AsmName}");
            return;
        }

        //Debug.Assert(operand.Size == 8);

        _writer.Append($"push {(operand with { Size = 8 }).AsmName}");
        _rspParity = (_rspParity - 8) & 0xF;
    }
    private void Pop(Operand operand)
    {
        if (IsFloatOperand(operand))
        {
            _writer.Append($"movdqu  {operand.AsmName}, [rsp]");
            AddRsp(16);
            return;
        }

        //Debug.Assert(operand.Size == 8);

        _writer.Append($"pop {(operand with { Size = 8 }).AsmName}");
        _rspParity = (_rspParity - 8) & 0xF;
    }



    private int AllocateTemp(int bytes)
    {
        _tempCursor += bytes;
        var off = _localsSize + _tempCursor;
        _tempMax = Math.Max(_tempMax, _tempCursor);
        return off;
    }
    private int AllocateLargeParameter(int bytes)
    {
        _tempCursor += bytes;
        var off = _localsSize + _tempCursor;
        _tempMax = Math.Max(_tempMax, _tempCursor);
        return off;
    }




    private void SubRsp(int bytes)
    {
        _writer.Append($"sub rsp, {bytes}");
        _rspParity = (_rspParity - bytes) & 0xF;
    }
    private void AddRsp(int bytes)
    {
        _writer.Append($"add rsp, {bytes}");
        _rspParity = (_rspParity + bytes) & 0xF;
    }
}
