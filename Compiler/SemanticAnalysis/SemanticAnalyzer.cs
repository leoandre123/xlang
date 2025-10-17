using System;
using System.Data.SqlTypes;
using System.Diagnostics;
using System.Xml.Linq;
using xlang.Compiler.Parsing;
using xlang.Compiler.Structures;
using xlang.Compiler.Structures.AST;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace xlang.Compiler.SemanticAnalysis;

public class TypeTable
{
    private Dictionary<string, TypeSymbol> _types = [];

    public bool TryGet(string fqn, out TypeSymbol symbol)
    {
        if (Types.BuiltIn.Any(x => x.Name == fqn))
        {
            symbol = Types.BuiltIn.First(x => x.Name == fqn);
            return true;
        }

        return _types.TryGetValue(fqn, out symbol);
    }

    public void DeclareNamed(string fqn, TypeSymbol symbol)
    {
        _types.Add(fqn, symbol);
    }
}

public record ArithmeticConst(bool IsConst, int IntValue)
{
    public bool IsBool { get; init; } = false;
    public bool BoolValue => IntValue != 0;
    public static ArithmeticConst Int(int val) => new(true, val);
    public static ArithmeticConst Bool(bool val) => new(true, val ? 1 : 0) { IsBool = true };
    public static ArithmeticConst Unknown() => new(false, 0);
}
public class ConstEnvironment
{
    public Dictionary<SymbolId, ArithmeticConst> Map = new();
    public ConstEnvironment Clone() => new() { Map = new(Map) };
}

public class SemanticAnalyzer
{
    private const int Alignment = 8;

    private List<Module> _programs;
    private TypeTable _types = new();

    private Scope _globalScope;
    private Scope _currentScope;
    private ScopeNode _currentScopeNode;
    private Module _currentModule;

    private SemanticModel _model;

    private FunctionSymbol _currentFunction;
    private SymbolId _currentClass;

    private int _nextOffset;
    private bool _usesCalls;
    private int _fieldOffset;

    private bool _usesBreaks;

    private readonly Stack<ConstEnvironment> _constStack = new([new ConstEnvironment()]);
    private ConstEnvironment ConstEnv => _constStack.Peek();

    private class UsageData
    {
        public int Reads { get; set; } = 0;
        public int Writes { get; set; } = 0;
    };

    private Dictionary<SymbolId, UsageData> _usage = [];
    //private Dictionary<SymbolId, Symbol> _symbols = [];

    public SemanticAnalyzer(List<Module> programs)
    {
        _programs = programs;
    }
    public SemanticModel Analyze()
    {
        Logger.LogDebug("Analyzing...");
        _globalScope = new Scope(null);
        _model = new SemanticModel();

        GenerateBuiltins();
        AnalyzeDeclarations();
        ResolveAllTypes(_model.ScopeTreeBaseNode);
        RunLayout();
        AnalyzeImplementations();

        //if(useWarnings)
        AnalyzeWarnings();


        return _model;
    }

    [Obsolete]
    public SemanticModel GetModel() => _model;

    /* PASS 0.5 - Generating built-in symbols */
    private void GenerateBuiltins()
    {
        //AddSymbol(new ClassSymbol()
        //{
        //
        //},-1);
    }

    /* PASS 1 (Scopes pass - After this pass all types exist by name) */
    private void AnalyzeDeclarations()
    {
        Logger.LogTrace("Analyzing declarations...");


        foreach (var prog in _programs)
        {
            //_currentScope = _globalScope;
            _currentScopeNode = _model.ScopeTreeBaseNode;
            _currentModule = prog;

            AnalyzeScopeDeclarations(null, prog.Program.Body);
        }
    }
    private void AnalyzeScopeDeclarations(string? name, ScopeBody scope)
    {
        //_currentScope = new Scope(_currentScope);
        var scopeBefore = _currentScopeNode;
        _currentScopeNode = _currentScopeNode.GetOrCreate(name);

        if (_currentScopeNode.Symbol == null)
        {
            var scopeSymbol = new ScopeSymbol()
            {
                Name = _currentScopeNode.Name,
                FullyQualifiedName = _currentScopeNode.GetFullName(),
                OwnerModule = _currentModule,
                Span = scope.Span
            };
            AddSymbol(scopeSymbol, scope.Id);

            _currentScopeNode.SetSymbol(scopeSymbol.Id);
        }

        for (var i = 0; i < scope.Scopes.Count; i++)
        {
            AnalyzeScopeDeclarations(scope.Scopes[i].Name, scope.Scopes[i].Body);
        }

        foreach (var alias in scope.Aliases)
        {
            AnalyzeAliasDeclaration(alias);
        }

        foreach (var cls in scope.Classes)
        {
            AnalyzeClassDeclaration(cls);
        }

        foreach (var func in scope.Functions)
        {
            AnalyzeFunctionDeclaration(func);
        }

        _currentScopeNode = scopeBefore;
    }

    private void AnalyzeAliasDeclaration(AliasDeclaration alias)
    {
        var parentType = ResolveTypeOrUnknown(alias.Type);
        var aliasType = parentType with { Name = alias.Name, FullyQualifiedName = _currentScopeNode.GetFullName(alias.Name) };

        _types.DeclareNamed(_currentScopeNode.GetFullName(alias.Name), aliasType);
        var aliasSymbol = new AliasSymbol
        {
            OwnerModule = _currentModule,
            Type = aliasType,
            Name = alias.Name,
            Span = alias.Span
        };

        AddSymbol(aliasSymbol, alias.Id);
        RecordSymbol(aliasSymbol.Id);

        _currentScopeNode.Symbols.Add(aliasSymbol.Id);
    }
    private void AnalyzeClassDeclaration(ClassDeclaration classDeclaration)
    {
        _currentScopeNode = _currentScopeNode.AddClassChild(classDeclaration.Name);

        List<SymbolId> memberSymbolIds = [];

        foreach (var member in classDeclaration.Members)
        {
            if (member is ClassField memberVariable)
            {
                var type = ResolveTypeOrUnknown(memberVariable.Type);
                var fieldSym = new FieldSymbol
                {
                    OwnerModule = _currentModule,
                    Type = type,
                    Offset = -1,
                    Name = memberVariable.Name,
                    Span = memberVariable.Span,
                    AccessType = memberVariable.AccessType
                };
                memberSymbolIds.Add(fieldSym.Id);
                AddSymbol(fieldSym, memberVariable.Id);
                RecordSymbol(fieldSym.Id);
            }
            else if (member is FunctionDeclaration function)
            {
                var returnType = ResolveTypeOrUnknown(function.ReturnType);
                List<ParameterSymbol> paramSyms =
                [
                    new()
                    {
                        OwnerModule = _currentModule,
                        Type = new UnresolvedTypeSymbol($"{classDeclaration.Name}"),
                        Index = 0,
                        Name = "this",
                        Span = function.Span
                    }
                ];
                AddSymbol(paramSyms[0], -1);
                RecordSymbol(paramSyms[0].Id);

                for (var i = 0; i < function.Parameters.Count; i++)
                {
                    var param = function.Parameters[i];
                    var type = ResolveTypeOrUnknown(param.Type);
                    var paramSym = new ParameterSymbol
                    {
                        OwnerModule = _currentModule,
                        Type = type,
                        Index = i + 1,
                        Name = param.Name,
                        Span = function.Parameters[i].Span
                    };
                    paramSyms.Add(paramSym);
                    AddSymbol(paramSym, function.Parameters[i].Id);
                    RecordSymbol(paramSym.Id);
                }

                var methodSymbol = new MethodSymbol
                {
                    OwnerModule = _currentModule,
                    Type = returnType,
                    Name = function.Name,
                    FullyQualifiedName = _currentScopeNode.GetFullName(function.Name),
                    Parameters = paramSyms.Select(x => x.Id).ToList(),
                    AccessType = function.AccessType,
                    Span = function.Span
                };
                AddSymbol(methodSymbol, function.Id);
                RecordSymbol(methodSymbol.Id);
                memberSymbolIds.Add(methodSymbol.Id);
            }
        }

        var classType = new ClassTypeSymbol(classDeclaration.Name, -1, -1, _currentScopeNode.GetFullName());

        _types.DeclareNamed(_currentScopeNode.GetFullName(), classType);

        var classSymbol = new ClassSymbol
        {
            Type = classType,
            //Fields = fieldSymbols,
            //Methods = methodSymbols,
            //Constructors = constructorSymbols,
            Members = memberSymbolIds,
            Name = classDeclaration.Name,
            OwnerModule = _currentModule,
            Span = classDeclaration.Span
        };
        ((classSymbol.Type as ClassTypeSymbol)!).DeclaringSymbol = classSymbol;
        AddSymbol(classSymbol, classDeclaration.Id);
        RecordSymbol(classSymbol.Id);

        _currentScopeNode.SetSymbol(classSymbol.Id);
        // _currentScopeNode.Symbols.AddRange(fieldSymbols.Select(x=>x.Id));
        // _currentScopeNode.Symbols.AddRange(constructorSymbols.Select(x => x.Id));
        // _currentScopeNode.Symbols.AddRange(methodSymbols.Select(x => x.Id));
        _currentScopeNode.Symbols.AddRange(memberSymbolIds);

        _currentScopeNode = _currentScopeNode.Parent!;
    }
    private void AnalyzeFunctionDeclaration(FunctionDeclaration function)
    {

        var returnType = ResolveTypeOrUnknown(function.ReturnType);

        var paramSyms = new List<SymbolId>();
        for (var i = 0; i < function.Parameters.Count; i++)
        {
            var par = function.Parameters[i];
            var type = ResolveTypeOrUnknown(par.Type);
            var ps = new ParameterSymbol
            {
                OwnerModule = _currentModule,
                Name = par.Name,
                Type = type,
                Index = i,
                Span = function.Parameters[i].Span
            };
            AddSymbol(ps, function.Parameters[i].Id);
            RecordSymbol(ps.Id);
            paramSyms.Add(ps.Id);
        }

        Symbol funcSym;
        if (function.IsExtern)
        {
            funcSym = new ExternalFunctionSymbol
            {
                OwnerModule = _currentModule,
                Type = returnType,
                Name = function.Name,
                FullyQualifiedName = function.Name,
                Parameters = paramSyms,
                Span = function.Span
            };

            //_model.ExternalFunctions[_currentModule.Id].Add(funcSym.Id);
        }
        else
        {
            funcSym = new FunctionSymbol
            {
                OwnerModule = _currentModule,
                AccessType = function.AccessType,
                Type = returnType,
                Name = function.Name,
                FullyQualifiedName = function.IsExtern ? function.Name : _currentScopeNode.GetFullName(function.Name),
                Parameters = paramSyms,
                Span = function.Span
            };
            if (function is { Name: "Main", IsInstance: false, IsExtern: false })
            {
                _model.EntryPoint = funcSym.Id;
            }
        }

        AddSymbol(funcSym, function.Id);
        RecordSymbol(funcSym.Id);
        _currentScopeNode.Symbols.Add(funcSym.Id);
    }


    /* PASS 1.5 (Resolve pass - All types are now bound to a TypeSymbol) */
    public void ResolveAllTypes(ScopeNode node)
    {
        _currentScopeNode = node;
        foreach (var child in node.Children)
        {
            ResolveAllTypes(child);
        }

        foreach (var symbol in node.GetAllSymbols(false))
        {
            if (symbol is FunctionSymbol fs)
            {
                //var newParams = new List<ParameterSymbol>();

                foreach (var par in fs.Parameters)
                {
                    var parSym = _model.GetSymbol<ParameterSymbol>(par);
                    if (parSym.Type is UnresolvedTypeSymbol)
                        UpdateSymbol(parSym with { Type = ResolveType(parSym.Type.Name, parSym.Span) });

                }
                var paramFqns = fs.Parameters.Select(x => _model.GetSymbol<ParameterSymbol>(x).Type.FullyQualifiedName);
                var fqn = $"{node.GetFullName(fs.Name)}({string.Join(',', paramFqns)})";
                UpdateSymbol(fs with { FullyQualifiedName = fqn });
            }

            if (symbol is SymbolWithType { Type: UnresolvedTypeSymbol } swt)
            {
                if (symbol is MethodSymbol ms && swt.Type.Name == "")
                {
                    UpdateSymbol(swt with { Type = Types.Void /**/ });
                }
                else
                {
                    UpdateSymbol(swt with { Type = ResolveType(swt.Type.Name, swt.Span) });
                }
            }
        }
    }


    /* PASS 1.6 (Layout pass - All TypeSymbols should now have the correct size info) */
    public void RunLayout()
    {
        var classSymbols = _model.ScopeTreeBaseNode.GetAllSymbols(true).OfType<ClassSymbol>();

        //1. Check loops
        //2. Order before layout


        foreach (var cls in classSymbols)
        {
            LayoutClass(cls);
        }
    }

    public void LayoutClass(ClassSymbol classSymbol)
    {
        var classTypeSymbol = (classSymbol.Type as ClassTypeSymbol)!;

        var offset = 0;
        var maxAlign = 1;

        foreach (var member in classSymbol.Members)
        {
            var symbol = _model.GetSymbol(member);
            if (symbol is FieldSymbol field)
            {
                offset = Align(offset, field.Type.Alignment);
                classTypeSymbol.FieldOffset.Add(field.Id, offset);
                UpdateSymbol(field with { Offset = offset });
                offset += field.Type.Size;
                maxAlign = Math.Max(maxAlign, field.Type.Alignment);
            }
        }

        //classTypeSymbol.ClassSize = offset;
        //classTypeSymbol.ClassAlignment = maxAlign;
    }


    /* PASS 2 (Implementation pass) */
    public void AnalyzeImplementations()
    {
        Logger.LogTrace("Analyzing implementations...");
        foreach (var module in _programs)
        {
            _currentModule = module;
            _currentScope = _globalScope;
            _currentScopeNode = _model.ScopeTreeBaseNode;

            AnalyzeScopeImplementations(null, module.Program.Body);
        }
    }

    public void AnalyzeScopeImplementations(string? name, ScopeBody scope)
    {
        var prevNode = _currentScopeNode;
        var prevScope = _currentScope;
        if (name != null)
        {
            _currentScopeNode = _currentScopeNode.GetNode(name)!;
            _currentScope = new Scope(_currentScope);
        }


        foreach (var s in scope.Scopes)
        {
            AnalyzeScopeImplementations(s.Name, s.Body);
        }

        foreach (var cls in scope.Classes)
        {
            AnalyzeClassImplementation(cls);
        }

        foreach (var function in scope.Functions)
        {
            if (function.IsExtern) continue;
            AnalyzeFunctionImplementation(function);
        }

        _currentScope = prevScope;
        _currentScopeNode = prevNode;
    }


    private void AnalyzeClassImplementation(ClassDeclaration classDeclaration)
    {
        var prevNode = _currentScopeNode;
        var prevScope = _currentScope;

        _currentScopeNode = _currentScopeNode.GetNode(classDeclaration.Name)!;
        _currentScope = new Scope(_currentScope);

        foreach (var member in classDeclaration.Members)
        {
            if (member is FunctionDeclaration function)
            {
                AnalyzeFunctionImplementation(function);
            }
            else if (member is ClassField classField)
            {
                AnalyzeClassField(classField);
            }
            else
            {
                throw new NotSupportedException();
            }
        }
        _currentScope = prevScope;
        _currentScopeNode = prevNode;
    }
    private void AnalyzeFunctionImplementation(FunctionDeclaration function)
    {
        var functionSymbol = _model.GetSymbol<FunctionSymbol>(function.Id);//(_currentScopeNode.GetSymbol(function.Name)?.symbol as FunctionSymbol)!;


        _constStack.Push(ConstEnv.Clone());
        //RecordSymbol(functionSymbol.Id);

        _currentFunction = functionSymbol;
        _nextOffset = 0;
        _usesCalls = false;

        _currentScope = new Scope(_currentScope);

        for (var i = 0; i < functionSymbol.Parameters.Count; i++)
        {
            if (!_currentScope.TryDeclare(_model.GetSymbol(functionSymbol.Parameters[i])))
                throw new SemanticException($"Parameter '{_model.GetSymbol(functionSymbol.Parameters[i]).Name}' is duplicated.", function.Span, _currentModule.File);
        }

        if (!function.IsExtern)
        {
            AnalyzeBlock(function.Body!);
        }


        _model.FunctionData[functionSymbol.Id] = new FunctionMetaData
        {
            IsLeaf = !_usesCalls,
            LocalsSize = Align(_nextOffset,
                16),
            FullyQualifiedName = null
        };


        _constStack.Pop();
        _currentScope = _currentScope.Parent!;
    }
    private void AnalyzeClass(ClassDeclaration classDeclaration)
    {
        // var size = 0;
        //
        //
        //
        // foreach (var member in classDeclaration.Members)
        // {
        //     if (member is ClassMemberVariable memberVariable)
        //     {
        //         var memberType = ResolveType(memberVariable.Type, memberVariable.Span);
        //
        //         size += memberType.Size;
        //     }
        // }
        //
        // var typeSymbol = new ClassTypeSymbol(classDeclaration.Name, size);
        //
        //
        // List<FieldSymbol> fieldSymbols = [];
        // List<MethodSymbol> methodSymbols = [];
        // List<ConstructorSymbol> constructorSymbols = [];
        //
        // foreach (var member in classDeclaration.Members)
        // {
        //     if (member is ClassMemberVariable memberVariable)
        //     {
        //         var type = ResolveType(memberVariable.Type, memberVariable.Span);
        //         var offset = AllocSlot(type);
        //         var fieldSym = new FieldSymbol(memberVariable.Name, type, offset);
        //
        //         if (memberVariable.Initial != null)
        //         {
        //             var initInfo = ResolveExpressionType(memberVariable.Initial);
        //
        //             if (!IsTypeAssignableFrom(type, initInfo.Type))
        //             {
        //                 throw new SemanticException($"Cannot assign {initInfo.Type} to {type}", memberVariable.Span);
        //             }
        //         }
        //         fieldSymbols.Add(fieldSym);
        //         _model.FieldSymbols[memberVariable.Id] = fieldSym;
        //     }
        //     else if (member is ClassMethodDeclaration methodDeclaration)
        //     {
        //         var returnType = ResolveType(methodDeclaration.ReturnType, methodDeclaration.Span);
        //
        //
        //         var paramSyms = new List<ParameterSymbol>();
        //         paramSyms.Add(new ParameterSymbol("this", new PointerTypeSymbol(typeSymbol), 0));
        //         _currentScope.TryDeclare(paramSyms[0]);
        //
        //
        //         for (var i = 0; i < methodDeclaration.Parameters.Count; i++)
        //         {
        //             var param = methodDeclaration.Parameters[i];
        //             var type = ResolveType(param.Type, param.Span);
        //
        //             var paramSym = new ParameterSymbol(param.Name, type, i + 1);
        //             if (!_currentScope.TryDeclare(paramSym))
        //                 throw new SemanticException($"Parameter '{param.Name}' has already been declared.", param.Span);
        //             paramSyms.Add(paramSym);
        //
        //             _model.ParameterSymbols[param.Id] = paramSym;
        //         }
        //
        //         var methodSymbol = new MethodSymbol(methodDeclaration.Name, Utils.Utils.GetFullFunctionName(_currentScopeNode.GetFullName(), methodDeclaration.Name), returnType, paramSyms);
        //         methodSymbols.Add(methodSymbol);
        //         _model.MethodSymbols[methodDeclaration.Id] = methodSymbol;
        //
        //         if (!_currentScope.TryDeclare(methodSymbol))
        //             throw new SemanticException($"Method {methodSymbol.Name} has already been declared", classDeclaration.Span);
        //     }
        //     else if (member is ClassConstructor constructor)
        //     {
        //         var paramSyms = new List<ParameterSymbol>();
        //
        //         paramSyms.Add(new ParameterSymbol("this", new PointerTypeSymbol(typeSymbol), 0));
        //         for (var i = 0; i < constructor.Parameters.Count; i++)
        //         {
        //             var param = constructor.Parameters[i];
        //             var type = ResolveType(param.Type, param.Span);
        //
        //             var offset = 0;
        //             if (i > 3)
        //                 offset = AllocSlot(type);
        //
        //             var paramSym = new ParameterSymbol(param.Name, type, i + 1);
        //             if (!_currentScope.TryDeclare(paramSym))
        //                 throw new SemanticException($"Parameter '{param.Name}' has already been declared.", param.Span);
        //             paramSyms.Add(paramSym);
        //
        //             _model.ParameterSymbols[param.Id] = paramSym;
        //         }
        //
        //         var constructorSymbol = new ConstructorSymbol(classDeclaration.Name, Utils.Utils.GetFullFunctionName(_currentScopeNode.GetFullName(), classDeclaration.Name), paramSyms);
        //
        //         constructorSymbols.Add(constructorSymbol);
        //         //if (!_globalScope.TryDeclare(classSymbol))
        //         //    throw new SemanticException($"Class {classSymbol.Name} has already been declared", classDeclaration.Span);
        //     }
        //
        // }
        //
        //
        // var classSymbol = new ClassSymbol(classDeclaration.Name, typeSymbol, fieldSymbols, methodSymbols, constructorSymbols);
        //
        // if (!_globalScope.TryDeclare(classSymbol))
        //     throw new SemanticException($"Class {classSymbol.Name} has already been declared", classDeclaration.Span);
        //
        //
        //
        // _model.ClassData[typeSymbol] = new ClassMetaData(size, classDeclaration.Members);
        // _model.ClassTypeSymbols.Add(typeSymbol);
    }

    private void AnalyzeClassField(ClassField field)
    {
        var fieldSym = (LookupVariable(field.Name) as FieldSymbol)!;
        if (!_currentScope.TryDeclare(fieldSym))
            throw new SemanticException($"Member '{fieldSym.Name}' has already been declared in this scope.", field.Span, _currentModule.File);
    }



    /* PASS 3 (Code analysis - warning generation) */
    public void AnalyzeWarnings()
    {
        foreach (var usage in _usage)
        {
            var symbol = _model.Symbols[usage.Key];

            if (usage.Value is { Reads: 0, Writes: 0 })
            {
                var code = symbol switch
                {
                    CallableSymbol => DiagnosticCode.UnusedFunction,
                    ClassSymbol => DiagnosticCode.UnusedClass,
                    FieldSymbol => DiagnosticCode.UnusedField,
                    ParameterSymbol => DiagnosticCode.UnusedParameter,
                    AliasSymbol => DiagnosticCode.UnusedAlias,
                    VariableSymbol => DiagnosticCode.UnusedLocal,

                    _ => throw new ArgumentOutOfRangeException()
                };


                _model.Diagnostics.Add(new Diagnostic
                {
                    Code = code,
                    Severity = DiagnosticSeverity.Warning,
                    Message = $"{symbol.GetSymbolTypeName()} '{symbol.Name}' is never used.",
                    ModuleId = symbol.OwnerModule.Id,
                    Span = symbol.Span
                });
            }
            else if (usage.Value is { Reads: 0 })
            {
                var code = symbol switch
                {
                    FieldSymbol => DiagnosticCode.NeverReadField,
                    ParameterSymbol => DiagnosticCode.NeverReadParameter,
                    VariableSymbol => DiagnosticCode.NeverReadLocal,

                    _ => throw new ArgumentOutOfRangeException()
                };

                _model.Diagnostics.Add(new Diagnostic
                {
                    Code = code,
                    Severity = DiagnosticSeverity.Warning,
                    Message = $"{symbol.GetSymbolTypeName()} '{symbol.Name}' is never read.",
                    ModuleId = symbol.OwnerModule.Id,
                    Span = symbol.Span
                });
            }

        }
    }




    private void AnalyzeBlock(Block block)
    {
        _currentScope = new Scope(_currentScope);
        _constStack.Push(ConstEnv.Clone());
        foreach (var statement in block.Statements)
        {
            AnalyzeStatement(statement);
        }

        _constStack.Pop();
        _currentScope = _currentScope.Parent ?? throw new Exception("No outer scope");
    }
    private void AnalyzeStatement(Statement statement)
    {
        switch (statement)
        {
            case VariableDeclaration variableDeclaration:
                {
                    AnalyzeVariableDeclaration(variableDeclaration);
                    break;
                }
            case IfStatement ifStatement:
                {
                    var expInfo = ResolveExpressionType(ifStatement.Expression);
                    if (expInfo.Type != Types.Bool)
                    {
                        throw new SemanticException($"Expression must be of type 'bool'", ifStatement.Span, _currentModule.File);
                    }

                    var evalConst = EvalConst(ifStatement.Expression);

                    if (evalConst.IsConst)
                    {
                        _model.Diagnostics.Add(new Diagnostic
                        {
                            Code = evalConst.BoolValue ? DiagnosticCode.AlwaysTrue : DiagnosticCode.AlwaysFalse,
                            Severity = DiagnosticSeverity.Warning,
                            Message = $"If will {(evalConst.BoolValue ? "always" : "never")} run",
                            ModuleId = _currentModule.Id,
                            Span = ifStatement.Expression.Span
                        });
                    }

                    AnalyzeStatement(ifStatement.Body);
                    if (ifStatement.Else != null)
                        AnalyzeStatement(ifStatement.Else);

                    break;
                }
            case WhileStatement whileStatement:
                {
                    var expInfo = ResolveExpressionType(whileStatement.Expression);
                    if (expInfo.Type != Types.Bool)
                    {
                        throw new SemanticException($"Expression must be of type 'bool'", whileStatement.Span, _currentModule.File);
                    }

                    var evalConst = EvalConst(whileStatement.Expression);

                    if (evalConst is { IsConst: true, BoolValue: false })
                    {
                        _model.Diagnostics.Add(new Diagnostic
                        {
                            Code = DiagnosticCode.NeverRun,
                            Severity = DiagnosticSeverity.Warning,
                            Message = $"While will never run",
                            ModuleId = _currentModule.Id,
                            Span = whileStatement.Expression.Span
                        });
                    }

                    var oldBreakVal = _usesBreaks;
                    _usesBreaks = false;
                    AnalyzeStatement(whileStatement.Body);
                    _usesBreaks = oldBreakVal;

                    break;
                }
            case ForStatement forStatement:
                {
                    _currentScope = new Scope(_currentScope);
                    AnalyzeStatement(forStatement.Initial);
                    var expInfo = ResolveExpressionType(forStatement.Expression);
                    if (expInfo.Type != Types.Bool)
                    {
                        throw new SemanticException($"Expression must be of type 'bool'", forStatement.Span, _currentModule.File);
                    }
                    AnalyzeStatement(forStatement.Increment);
                    AnalyzeStatement(forStatement.Body);

                    _currentScope = _currentScope.Parent!;
                    break;
                }
            case Expression expression:
                {
                    ResolveExpressionType(expression);
                    break;
                }
            case Block block:
                {
                    AnalyzeBlock(block);
                    break;
                }
            case Break:
                {
                    _usesBreaks = true;
                    break;
                };
            case Return returnStatement:
                {
                    var expressionType = returnStatement.Expression == null
                        ? Types.Void
                        : ResolveExpressionType(returnStatement.Expression).Type;

                    if (returnStatement.Expression is VariableExpression varExpr)
                    {
                        RecordRead(LookupVariable(varExpr.Name).Id);
                    }

                    if (!IsTypeAssignableFrom(_currentFunction.Type, expressionType))
                    {
                        throw new SemanticException($"Cannot assign '{expressionType}' to '{_currentFunction.Type}'", returnStatement.Span, _currentModule.File);
                    }

                    break;
                };
            default: throw new Exception($"Unhandled statement '{statement.GetType()}'");
        }
    }
    private void AnalyzeVariableDeclaration(VariableDeclaration declaration)
    {
        var count = 1;
        var type = ResolveType(declaration.Type, declaration.Span);



        if (declaration is ArrayDeclaration arr)
        {
            //count = ResolveConstantExpression(arr);
            var cnst = EvalConst(arr.Size);

            if (!cnst.IsConst)
                throw new SemanticException("Arrays need to be of constant size", arr.Size.Span, _currentModule.File);

            count = cnst.IntValue;

            type = new ArrayTypeSymbol(type);
        }



        var offset = AllocSlot(type, count);
        var varSym = new VariableSymbol
        {
            OwnerModule = _currentModule,
            Name = declaration.Name,
            Type = type,
            Count = count,
            Offset = offset,
            Span = declaration.Span
        };
        if (!_currentScope.TryDeclare(varSym))
            throw new SemanticException($"Variable '{declaration.Name}' has already been declared in this scope.", declaration.Span, _currentModule.File);

        AddSymbol(varSym, declaration.Id);
        RecordSymbol(varSym.Id);




        if (declaration.Initial != null)
        {
            if (type is ClassTypeSymbol classTypeSymbol && declaration.Initial is CallExpression call)
            {
                var classSymbol = LookupClass(classTypeSymbol.Name);//(LookUpSymbol(classTypeSymbol.Name).FirstOrDefault() as ClassSymbol)!;

                var args = call.Arguments.Select(x => ResolveExpressionType(x).Type).ToList();
                args.Insert(0, classTypeSymbol);

                //if (call.Name == classSymbol.Name)
                //{
                //    var constructorSymbol = classSymbol.Constructors.Single(x => AreParametersAssignableFrom(x.Parameters, args));
                //
                //    //_model.CallableSymbols[call.Id] = constructorSymbol;
                //    UseSymbol(classSymbol, call.Id);
                //    return;
                //}
            }

            var initInfo = ResolveExpressionType(declaration.Initial);

            if (!IsTypeAssignableFrom(type, initInfo.Type))
            {
                throw new SemanticException($"Cannot assign {initInfo.Type} to {type}", declaration.Span, _currentModule.File);
            }

            ConstEnv.Map[varSym.Id] = EvalConst(declaration.Initial);
        }
    }
    private ExpressionInfo ResolveExpressionType(Expression expression, TypeSymbol? expectedType = null)
    {
        var t = expression switch
        {
            IntegerLiteralExpression => new ExpressionInfo(Types.Int, ValueCategory.RValue, false),
            FloatLiteralExpression f => ResolveFloatLiteralType(f),
            BooleanLiteralExpression => new ExpressionInfo(Types.Bool, ValueCategory.RValue, false),
            StringLiteralExpression s => ResolveStringLiteralType(s),
            StringInterpolationExpression si => ResolveStringInterpolationType(si),

            VariableExpression v => ResolveVariable(v),

            UnaryExpression u => ResolveUnaryType(u),
            BinaryExpression b => ResolveBinaryType(b),

            CallExpression c => ResolveCallType(c),
            IndexExpression i => ResolveIndexType(i),
            MemberAccessExpression a => ResolveAccessType(a),
            MemberCallExpression mc => ResolveMemberCallType(mc),
            ArrayInitializerListExpression ai => ResolveArrayInitializerList(ai),
            MakeExpression mk => ResolveMakeExpression(mk),

            _ => throw new Exception($"Could not resolve type of expression '{expression.GetType()}'")
        };

        _model.ExpressionData[expression.Id] = t;
        return t;
    }

    private ExpressionInfo ResolveUnaryType(UnaryExpression expression)
    {
        var operandInfo = ResolveExpressionType(expression.Operand);

        if (expression.Operator.IsAssigning && operandInfo.Category == ValueCategory.RValue)
        {
            throw new SemanticException("Value is non-addressable", expression.Span, _currentModule.File);
        }

        var returnType = expression.Operator.GetUnaryOperationReturnType(operandInfo.Type);

        if (returnType == null)
        {
            throw new SemanticException($"Operator '{expression.Operator.Symbol}' cannot be used on operand of type '{operandInfo.Type.Name}'", expression.Span, _currentModule.File);
        }

        if (expression.Operand is VariableExpression varExpr)
        {
            var symbol = LookupVariable(varExpr.Name);
            RecordRead(symbol!.Id);
            if (expression.Operator.IsAssigning)
            {
                RecordWrite(symbol.Id, EvalConst(expression));
            }
        }

        return expression.Operator.Type == OperatorType.Dereference ?
            new ExpressionInfo(returnType, ValueCategory.LValue, operandInfo.ContainsCall) :
            new ExpressionInfo(returnType, ValueCategory.RValue, operandInfo.ContainsCall);
    }
    private ExpressionInfo ResolveBinaryType(BinaryExpression expression)
    {
        var leftOperandInfo = ResolveExpressionType(expression.Left);
        var rightOperandInfo = ResolveExpressionType(expression.Right);

        if (expression.Left is VariableExpression varExprLeft)
        {
            var symbol = LookupVariable(varExprLeft.Name);
            RecordRead(symbol!.Id);
            if (expression.Operator.IsAssigning) RecordWrite(symbol.Id, EvalConst(expression));
        }
        if (expression.Right is VariableExpression varExprRight)
        {
            var symbol = LookupVariable(varExprRight.Name);
            RecordRead(symbol!.Id);
        }


        if (expression.Operator.IsAssigning && leftOperandInfo.Category == ValueCategory.RValue)
        {
            throw new SemanticException("Left hand side of assignment must be assignable", expression.Span, _currentModule.File);
        }

        //if (leftOperandInfo.Type == Types.String || rightOperandInfo.Type == Types.String)
        //{
        //    var list = new ConcatenationList([]);
        //
        //
        //    if (_model.ConcatenationList.TryGetValue(expression.Left.Id, out var leftConcatList))
        //        list.List.AddRange(leftConcatList.List);
        //    else
        //        list.List.Add(expression.Left);
        //
        //    if (_model.ConcatenationList.TryGetValue(expression.Right.Id, out var rightConcatList))
        //        list.List.AddRange(rightConcatList.List);
        //    else
        //        list.List.Add(expression.Right);
        //
        //    _model.ConcatenationList[expression.Id] = list;
        //  }


        if (expression.Operator.Type == OperatorType.Assign)
        {
            if (!IsTypeAssignableFrom(leftOperandInfo.Type, rightOperandInfo.Type))
            {
                throw new SemanticException($"Cannot assign type '{rightOperandInfo.Type}' to type '{leftOperandInfo.Type}'", expression.Span, _currentModule.File);
            }

            return leftOperandInfo with { Category = ValueCategory.RValue };
        }



        var returnType = expression.Operator.GetBinaryOperationReturnType(leftOperandInfo.Type, rightOperandInfo.Type);

        if (returnType == null)
        {
            throw new SemanticException($"Operator '{expression.Operator.Symbol}' cannot be used on operands of type '{leftOperandInfo.Type}' and '{rightOperandInfo.Type}'", expression.Span, _currentModule.File);
        }

        return new ExpressionInfo(returnType, ValueCategory.RValue, leftOperandInfo.ContainsCall || rightOperandInfo.ContainsCall);

    }
    private ExpressionInfo ResolveVariable(VariableExpression expression)
    {
        //var symbol = _currentScope.Lookup(expression.Name).FirstOrDefault();
        var symbol = LookupVariable(expression.Name);

        if (symbol is ScopeSymbol ss)
            return new ExpressionInfo(new ScopeTypeSymbol(ss.Name, ss.FullyQualifiedName), ValueCategory.RValue, false);

        if (symbol is ClassSymbol cs)
            return new ExpressionInfo(new ScopeTypeSymbol(cs.Name, cs.Type.FullyQualifiedName), ValueCategory.RValue, false);

        if (symbol == null)
            throw new SemanticException($"Variable with name '{expression.Name}' has not been declared.", expression.Span, _currentModule.File);

        if (symbol is not (VariableSymbol or ParameterSymbol or FieldSymbol))
        {
            throw new SemanticException($"'{expression.Name}' is not a variable or parameter.", expression.Span, _currentModule.File);
        }

        UseSymbol(symbol, expression.Id);

        return new ExpressionInfo((symbol as SymbolWithType)!.Type, ValueCategory.LValue, false);

    }
    private ExpressionInfo ResolveCallType(CallExpression call)
    {
        _usesCalls = true;

        List<TypeSymbol> argTypes = [];
        foreach (var t in call.Arguments)
        {
            var argInfo = ResolveExpressionType(t);
            argTypes.Add(argInfo.Type);
            if (t is VariableExpression varExpr) RecordRead(LookupVariable(varExpr.Name)!.Id);
        }

        var symbol = LookupCallable(call.Name, argTypes); //_currentScope.Lookup(call.Name);

        if (symbol == null)
            throw new SemanticException($"Function with name '{call.Name}' has not been declared.", call.Span, _currentModule.File);


        UseSymbol(symbol, call.Id);
        RecordRead(symbol.Id);

        return new ExpressionInfo(symbol.Type, ValueCategory.RValue, true);
    }

    private ExpressionInfo ResolveStringLiteralType(StringLiteralExpression stringLiteral)
    {
        var id = _model.LiteralConstants.FindIndex(x => x is StringConstant s && s.Text == stringLiteral.Value);
        if (id == -1)
        {
            _model.LiteralConstants.Add(new StringConstant(stringLiteral.Value));
            id = _model.LiteralConstants.Count - 1;
        }


        var stringType = LookupClass("string")?.Type ?? throw new Exception("string error");

        _model.LiteralConstantMap[stringLiteral.Id] = id;

        //TODO: Maybe contains call for allocation??
        return new ExpressionInfo(stringType, ValueCategory.RValue, false);
    }
    private ExpressionInfo ResolveFloatLiteralType(FloatLiteralExpression floatLiteral)
    {
        var id = _model.LiteralConstants.FindIndex(x => x is FloatConstant f && f.Value == floatLiteral.Value);
        if (id == -1)
        {
            _model.LiteralConstants.Add(new FloatConstant(floatLiteral.Value));
            id = _model.LiteralConstants.Count - 1;
        }

        _model.LiteralConstantMap[floatLiteral.Id] = id;
        return new ExpressionInfo(Types.Float, ValueCategory.RValue, false);
    }
    private ExpressionInfo ResolveIndexType(IndexExpression index)
    {
        var baseInfo = ResolveExpressionType(index.Base);
        var indInfo = ResolveExpressionType(index.Index);

        var baseType = baseInfo.Type;

        if (index.Base is VariableExpression varExpr)
        {
            var symbol = LookupVariable(varExpr.Name);
            if (symbol == null)
                throw new SemanticException($"Variable with name '{varExpr.Name}' has not been declared.", varExpr.Span, _currentModule.File);

            UseSymbol(symbol, index.Id);
            RecordRead(symbol.Id);
        }

        if (baseType is ArrayTypeSymbol arrType)
        {
            baseType = arrType.Type;
        }
        else if (baseType is PointerTypeSymbol ptrType)
        {
            baseType = ptrType.Type;
        }
        else
        {
            throw new SemanticException(
                $"Type '{baseType}' cannot be indexed. Only arrays types and pointers are valid", index.Base.Span, _currentModule.File);
        }

        if (!IsTypeAssignableFrom(Types.Int, indInfo.Type))
        {
            throw new SemanticException($"Type '{indInfo.Type}' cannot be used to index an array", index.Index.Span, _currentModule.File);
        }


        return new ExpressionInfo(baseType, ValueCategory.LValue, baseInfo.ContainsCall || indInfo.ContainsCall);
    }
    private ExpressionInfo ResolveAccessType(MemberAccessExpression access)
    {
        //if (access.Count > 1)
        //{
        //    ResolveExpressionType(access.Base);
        //}

        //var accessBase = GetExpressionBase(access.Base);
        var baseInfo = ResolveExpressionType(access.Base);

        if (access.Base is VariableExpression varExpr)
        {
            var symbol = LookupVariable(varExpr.Name) ??
                         throw new SemanticException($"Variable with name '{varExpr.Name}' has not been declared.", varExpr.Span, _currentModule.File);
            UseSymbol(symbol, access.Base.Id);
            RecordRead(symbol.Id);
        }

        if (baseInfo.Type is ClassTypeSymbol classTypeSymbol)
        {
            foreach (var member in classTypeSymbol.DeclaringSymbol.Members)
            {
                var memberSymbol = _model.GetSymbol<SymbolWithType>(member);
                if (memberSymbol.Name == access.Member)
                {
                    if (!IsMemberAccessible(classTypeSymbol.DeclaringSymbol, memberSymbol))
                        throw new SemanticException($"'{access.Member}' of '{classTypeSymbol.FullyQualifiedName}' is inaccessible", access.Span, _currentModule.File);
                    GetSymbolAccessType(memberSymbol);
                    UseSymbol(memberSymbol, access.Id);
                    return new ExpressionInfo(memberSymbol.Type, memberSymbol is FieldSymbol ? ValueCategory.LValue : ValueCategory.RValue, baseInfo.ContainsCall);
                }
            }
        }
        else if (baseInfo.Type is ArrayTypeSymbol)
        {
            switch (access.Member)
            {
                case "Length": return new ExpressionInfo(Types.Int, ValueCategory.RValue, baseInfo.ContainsCall);
            };
        }
        throw new SemanticException($"Type '{baseInfo.Type}' does not contain member '{access.Member}'", access.Span, _currentModule.File);
    }
    private ExpressionInfo ResolveMemberCallType(MemberCallExpression call)
    {
        _usesCalls = true;
        var baseType = ResolveExpressionType(call.Base);
        var args = call.Arguments.Select(x => ResolveExpressionType(x).Type).ToList();

        CallableSymbol callable;

        if (baseType.Type is ScopeTypeSymbol scopeTypeSymbol)
        {
            callable = LookupCallable($"{scopeTypeSymbol.FullyQualifiedName}.{call.Member}", args) ?? throw new SemanticException($"Cannot find callable '{call.Member}' in '{scopeTypeSymbol.FullyQualifiedName}'.", call.Span, _currentModule.File);
        }
        else if (baseType.Type is ClassTypeSymbol classTypeSymbol)
        {
            var classSymbol = LookupClass(baseType.Type.Name) ?? throw new SemanticException($"'{baseType.Type.Name}' is not a class.", call.Span, _currentModule.File);
            callable = _model.GetSymbol<MethodSymbol>(classSymbol.Members.FirstOrDefault(x => _model.GetSymbol(x) is MethodSymbol ms && ms.Name == call.Member));

            args.Insert(0, classTypeSymbol);
        }
        else
        {
            throw new SemanticException($"'{baseType.Type}' is not a class or scope.", call.Span, _currentModule.File);
        }




        if (callable == null)
            throw new SemanticException($"'{baseType.Type}' does not contain a method with the name '{call}'.", call.Span, _currentModule.File);




        //if (!AreParametersAssignableFrom(methodSymbol.Parameters, args))
        //{
        //    throw new SemanticException("Arguments do not match parameters", call.Span);
        //}

        UseSymbol(callable, call.Id);
        RecordRead(callable.Id);

        return new ExpressionInfo(callable.Type, ValueCategory.RValue, true);
    }
    private ExpressionInfo ResolveStringInterpolationType(StringInterpolationExpression interpolation)
    {
        var containsCall = false;
        foreach (var part in interpolation.Parts)
        {
            containsCall = containsCall || ResolveExpressionType(part).ContainsCall;
        }

        var stringType = LookupClass("string")?.Type ?? throw new Exception("string error");
        return new ExpressionInfo(stringType, ValueCategory.RValue, containsCall);
    }
    private ExpressionInfo ResolveArrayInitializerList(ArrayInitializerListExpression initializerList)
    {
        var type = Types.Void;
        var containsCall = false;
        foreach (var part in initializerList.Elements)
        {
            var info = ResolveExpressionType(part);
            containsCall = containsCall || info.ContainsCall;
            type = info.Type;
        }

        return new ExpressionInfo(new ArrayTypeSymbol(type), ValueCategory.RValue, containsCall);
    }
    private ExpressionInfo ResolveMakeExpression(MakeExpression makeExpression)
    {
        _usesCalls = true;
        IReadOnlyList<Expression> args;
        string callName;
        if (makeExpression.Call is CallExpression call)
        {
            args = call.Arguments;
            callName = call.Name;
        }
        else if (makeExpression.Call is MemberCallExpression memberCall)
        {
            args = memberCall.Arguments;

            var info = ResolveExpressionType(memberCall.Base);
            if (info.Type is not ScopeTypeSymbol scope)
                throw new SemanticException($"'{info.Type}' is not a scope", memberCall.Base.Span, _currentModule.File);
            callName = $"{scope.FullyQualifiedName}.{memberCall.Member}";
        }
        else
        {
            throw new SemanticException("'make' can only be used with constructors!", makeExpression.Span, _currentModule.File);
        }



        var classSymbol = LookupClass(callName) ?? throw new SemanticException($"No class named '{callName}'", makeExpression.Call.Span, _currentModule.File);


        List<TypeSymbol> argTypes = [classSymbol.Type];
        foreach (var t in args)
        {
            var argInfo = ResolveExpressionType(t);
            argTypes.Add(argInfo.Type);
            if (t is VariableExpression varExpr) RecordRead(LookupVariable(varExpr.Name)!.Id);
        }

        var constructorId = classSymbol.Members.FirstOrDefault(x => _model.GetSymbol(x) is CallableSymbol callSymbol && AreParametersAssignableFrom(callSymbol.Parameters, argTypes)) ?? throw new SemanticException($"No suitable constructor found '{callName}'", makeExpression.Call.Span, _currentModule.File);
        var constructor = _model.GetSymbol<CallableSymbol>(constructorId);

        UseSymbol(constructor, makeExpression.Call.Id);
        RecordRead(constructor.Id);
        UseSymbol(classSymbol, makeExpression.Id);

        //TODO: Är väl tekniskt sett en LValue men men
        _model.ExpressionData[makeExpression.Call.Id] = new ExpressionInfo(classSymbol.Type, ValueCategory.RValue, true);
        return new ExpressionInfo(classSymbol.Type, ValueCategory.RValue, true);
    }


    private void AddSymbol(Symbol symbol, int declarationId)
    {
        Debug.Assert(!_model.Symbols.ContainsKey(symbol.Id));
        _model.Symbols[symbol.Id] = symbol;
        if (declarationId != -1)
            _model.NodeToSymbolId[declarationId] = symbol.Id;
    }
    private void UpdateSymbol(Symbol symbol)
    {
        Debug.Assert(_model.Symbols.ContainsKey(symbol.Id));
        _model.Symbols[symbol.Id] = symbol;
    }
    private void UseSymbol(Symbol symbol, int usingNodeId)
    {
        Debug.Assert(_model.Symbols.ContainsKey(symbol.Id));
        _model.NodeToSymbolId[usingNodeId] = symbol.Id;

        if (symbol is ExternalFunctionSymbol || (symbol is CallableSymbol && symbol.OwnerModule.Id != _currentModule.Id))
        {
            if (_model.ExternalFunctions.TryGetValue(_currentModule.Id, out var value))
                value.Add(symbol.Id);
            else
                _model.ExternalFunctions[_currentModule.Id] = [symbol.Id];
        }
    }

    private bool IsMemberAccessible(ClassSymbol classSymbol, Symbol memberSymbol)
    {
        return GetSymbolAccessType(memberSymbol) switch
        {
            EAccessType.Public => true,
            EAccessType.Module => memberSymbol.OwnerModule.Id == _currentModule.Id,
            EAccessType.Private => memberSymbol.OwnerModule.Id == _currentModule.Id && classSymbol.Id == _currentClass,
            _ => throw new ArgumentOutOfRangeException()
        };
    }
    private EAccessType GetSymbolAccessType(Symbol symbol)
    {
        return symbol switch
        {
            ConstructorSymbol constructorSymbol => constructorSymbol.AccessType,
            MethodSymbol methodSymbol => methodSymbol.AccessType,
            FunctionSymbol functionSymbol => functionSymbol.AccessType,
            FieldSymbol fieldSymbol => fieldSymbol.AccessType,
            _ => throw new ArgumentOutOfRangeException(nameof(symbol))
        };
    }

    //private Expression GetExpressionBase(Expression expression)
    //{
    //    if (expression is not MemberAccessExpression member) return expression;
    //
    //    var count = member.Count;
    //
    //    
    //
    //    //GetExpressionBase()
    //}

    private bool IsTypeAssignableFrom(TypeSymbol to, TypeSymbol from)
    {
        //var baseTo = GetBaseType(to);
        //var baseFrom = GetBaseType(from);

        if (Types.IsBuiltInConversionAllowed(to, from)) return true;
        if (Types.IsImplicitConversionAllowed(to, from)) return true;
        return to == from;
    }
    private bool AreParametersAssignableFrom(IReadOnlyList<ParameterSymbol> parameters, List<TypeSymbol> arguments)
    {
        if (parameters.Count != arguments.Count) return false;

        for (var i = 0; i < parameters.Count; i++)
        {
            if (!IsTypeAssignableFrom(parameters[i].Type, arguments[i])) return false;
        }

        return true;
    }
    private bool AreParametersAssignableFrom(IReadOnlyList<SymbolId> parameters, List<TypeSymbol> arguments)
    {
        return AreParametersAssignableFrom(parameters.Select(_model.GetSymbol<ParameterSymbol>).ToList(), arguments);
    }

    private static int Align(int num, int alignment)
    {
        //return num + (alignment - num % alignment);
        return ((num + alignment - 1) / alignment) * alignment;
    }
    private int AllocSlot(TypeSymbol typeSymbol, int count = 1)
    {
        var first = -1;
        for (var i = 0; i < count; i++)
        {
            _nextOffset += typeSymbol.IsReferenceType ? 8 : typeSymbol.Size;
            _nextOffset = Align(_nextOffset, typeSymbol.Size);
            if (first == -1) first = _nextOffset;
        }

        return _nextOffset;
    }
    private int AllocField(TypeSymbol typeSymbol)
    {
        _nextOffset += typeSymbol.Size;
        _nextOffset = Align(_nextOffset, typeSymbol.Size);
        return _nextOffset;
    }

    private Symbol? LookupVariable(string name)
    {
        var scopeVar = _currentScope.LookupVariable(name);

        if (scopeVar != null)
            return scopeVar;

        var sym = _currentScopeNode.FindSymbolUp(name, SymbolType.All);

        if (sym != null)
        {
            return sym.Value.symbol;
        }

        foreach (var import in _currentModule.Program.Imports)
        {
            var scopeNode = _model.ScopeTreeBaseNode.GetNode(import.Scope) ?? throw new SemanticException("Could not resolve import", import.Span, _currentModule.File);
            sym = scopeNode.GetSymbol(name);
            if (sym != null)
            {
                return sym.Value.symbol;
            }
        }

        return null;
    }
    private ClassSymbol? LookupClass(string name)
    {
        var sym = _currentScopeNode.FindSymbolUp(name, SymbolType.TypeDeclaring);
        {
            if (sym?.symbol is ClassSymbol cls)
            {
                return cls;
            }
        }

        foreach (var import in _currentModule.Program.Imports)
        {
            var scopeNode = _model.ScopeTreeBaseNode.GetNode(import.Scope) ?? throw new SemanticException("Could not resolve import", import.Span, _currentModule.File);
            sym = scopeNode.GetSymbol(name);
            if (sym?.symbol is ClassSymbol cls)
            {
                return cls;
            }
        }

        return null;
    }
    private CallableSymbol? LookupCallable(string name, List<TypeSymbol> args)
    {
        var symbols = _currentScopeNode.FindManySymbolsUp(name, SymbolType.Callable);
        {
            foreach (var symbol in symbols)
            {
                if (symbol.symbol is not CallableSymbol callable)
                    continue;

                if (AreParametersAssignableFrom(callable.Parameters, args))
                    return callable;
            }
        }
        return null;
    }


    private TypeSymbol ResolveTypeOrUnknown(string typeName)
    {
        return TryResolveType(typeName) ?? new UnresolvedTypeSymbol(typeName);
    }
    private TypeSymbol ResolveType(string typeName, SourceSpan span)
    {
        return TryResolveType(typeName) ?? throw new SemanticException($"Could not resolve type '{typeName}'", span, _currentModule.File);
    }
    private TypeSymbol? TryResolveType(string typeName)
    {

        var name = typeName;
        var isPointer = false;
        if (typeName.EndsWith('*'))
        {
            isPointer = true;
            name = typeName[..^1];
        }

        // TRY DIRECTLY - FOR built ins
        if (_types.TryGet(name, out var builtIn))
            return isPointer ? new PointerTypeSymbol(builtIn) : builtIn;


        var result = new List<(string fqn, Symbol symbol)>();

        var sym = _currentScopeNode.FindSymbolUp(name, SymbolType.TypeDeclaring);

        if (sym?.symbol != null)
        {
            result.Add(sym.Value);
        }

        foreach (var import in _currentModule.Program.Imports)
        {
            var scopeNode = _model.ScopeTreeBaseNode.GetNode(import.Scope);// ?? throw new SemanticException("Could not resolve import", import.Span, _currentModule.File);
            if (scopeNode == null) continue;
            sym = scopeNode.GetSymbol(name);
            if (sym?.symbol != null)
            {
                result.Add(sym.Value);
            }
        }

        if (result.Count == 0)
            return null;

        if (_types.TryGet(result[0].fqn, out var typeSymbol))
            return isPointer ? new PointerTypeSymbol(typeSymbol) : typeSymbol;

        return null;
    }



    private bool AreUnsafeOperationsAllowed()
    {
        return false;
    }


    /* Diagnostics functions */
    public void RecordSymbol(SymbolId symbolId)
    {
        Debug.Assert(!_usage.ContainsKey(symbolId));
        _usage.Add(symbolId, new UsageData());
    }
    public void RecordRead(SymbolId symbolId)
    {
        Debug.Assert(_usage.ContainsKey(symbolId));
        _usage[symbolId].Reads++;
    }
    public void RecordWrite(SymbolId symbolId, ArithmeticConst arithmeticConst)
    {
        Debug.Assert(_usage.ContainsKey(symbolId));
        _usage[symbolId].Writes++;
        ConstEnv.Map[symbolId] = arithmeticConst;
    }


    ArithmeticConst EvalConst(Expression expression)
    {
        switch (expression)
        {
            case BooleanLiteralExpression ble: return new ArithmeticConst(true, ble.Value ? 1 : 0);
            case IntegerLiteralExpression ile: return new ArithmeticConst(true, ile.Value);
            case FloatLiteralExpression floatLiteralExpression: return ArithmeticConst.Unknown();
            case BinaryExpression binaryExpression:
                var left = EvalConst(binaryExpression.Left);
                var right = EvalConst(binaryExpression.Right);
                if (!left.IsConst && !right.IsConst) return new ArithmeticConst(false, 0);

                return binaryExpression.Operator.Type switch
                {
                    OperatorType.Add => ArithmeticConst.Int(left.IntValue + right.IntValue),
                    OperatorType.Sub => ArithmeticConst.Int(left.IntValue - right.IntValue),
                    OperatorType.Mul => ArithmeticConst.Int(left.IntValue * right.IntValue),
                    OperatorType.Div => ArithmeticConst.Int(left.IntValue / right.IntValue),
                    OperatorType.Mod => ArithmeticConst.Int(left.IntValue % right.IntValue),

                    OperatorType.Equal => ArithmeticConst.Bool(left.IntValue == right.IntValue),
                    OperatorType.NotEqual => ArithmeticConst.Bool(left.IntValue != right.IntValue),

                    OperatorType.Less => ArithmeticConst.Bool(left.IntValue < right.IntValue),
                    OperatorType.LessOrEqual => ArithmeticConst.Bool(left.IntValue <= right.IntValue),
                    OperatorType.Greater => ArithmeticConst.Bool(left.IntValue > right.IntValue),
                    OperatorType.GreaterOrEqual => ArithmeticConst.Bool(left.IntValue >= right.IntValue),

                    OperatorType.LogicalAnd => ArithmeticConst.Bool(left.BoolValue && right.BoolValue),
                    OperatorType.LogicalOr => ArithmeticConst.Bool(left.BoolValue || right.BoolValue),

                    OperatorType.Assign => ArithmeticConst.Int(right.IntValue),
                    OperatorType.AddAssign => ArithmeticConst.Int(left.IntValue + right.IntValue),
                    OperatorType.SubAssign => ArithmeticConst.Int(left.IntValue - right.IntValue),
                    OperatorType.MulAssign => ArithmeticConst.Int(left.IntValue * right.IntValue),
                    OperatorType.DivAssign => ArithmeticConst.Int(left.IntValue / right.IntValue),
                    OperatorType.ModAssign => ArithmeticConst.Int(left.IntValue % right.IntValue),
                    _ => ArithmeticConst.Unknown()
                };

            case UnaryExpression unaryExpression:
                var operand = EvalConst(unaryExpression.Operand);
                if (!operand.IsConst) return new ArithmeticConst(false, 0);

                return unaryExpression.Operator.Type switch
                {
                    OperatorType.PreIncrement => ArithmeticConst.Int(operand.IntValue + 1),
                    OperatorType.PreDecrement => ArithmeticConst.Int(operand.IntValue - 1),
                    OperatorType.PostIncrement => ArithmeticConst.Int(operand.IntValue),
                    OperatorType.PostDecrement => ArithmeticConst.Int(operand.IntValue),

                    OperatorType.Neg => ArithmeticConst.Int(-operand.IntValue),

                    OperatorType.LogicalNot => ArithmeticConst.Bool(!operand.BoolValue),
                    _ => ArithmeticConst.Unknown()
                };
            case VariableExpression variableExpression:
                var sym = LookupVariable(variableExpression.Name);
                return ConstEnv.Map.TryGetValue(sym!.Id, out var val) ? val : ArithmeticConst.Unknown();

            default: return ArithmeticConst.Unknown();
        }
    }
}
