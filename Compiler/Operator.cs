using xlang.Compiler.SemanticAnalysis;
using static xlang.Compiler.OperatorType;

namespace xlang.Compiler;

public enum OperatorType
{
    None,

    //Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,

    Neg,


    //Relational
    Equal,
    NotEqual,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,


    //Logical
    LogicalNot,
    LogicalAnd,
    LogicalOr,

    //Bitwise
    BitwiseNot,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,


    //Assignment
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,


    //
    Address,
    Dereference
}
public enum Associativity { Left, Right }
public class Operator(string symbol, OperatorType type, int precedence, Associativity associativity = Associativity.Left, bool isAssigning = false)
{

    public OperatorType Type { get; set; } = type;
    public string Symbol { get; set; } = symbol;
    public int Precedence { get; set; } = precedence;
    public Associativity Associativity { get; set; } = associativity;
    public bool IsAssigning { get; set; } = isAssigning;

    public Func<TypeSymbol, TypeSymbol?>? UnaryOperationMap;
    public Func<TypeSymbol, TypeSymbol, TypeSymbol?>? BinaryOperationMap;


    public Operator AddBinaryOperation(TypeSymbol operandsType)
    {
        BinaryOperationMap = (left, right) => ((operandsType == left) && right == left) ? left : null;
        return this;
    }
    public Operator AddUnaryOperation(TypeSymbol operandType)
    {
        UnaryOperationMap = (op) => operandType == op ? op : null;
        return this;
    }
    public Operator AddUnaryOperations(List<TypeSymbol> operandTypes)
    {
        UnaryOperationMap = (op) => operandTypes.Contains(op) ? op : null;
        return this;
    }
    public Operator AddBinaryOperations(List<TypeSymbol> operandTypes)
    {
        BinaryOperationMap = (left, right) => (operandTypes.Contains(left) && right == left) ? left : null;
        return this;
    }
    public Operator AddBinaryOperations(List<TypeSymbol> operandTypes, TypeSymbol returnType)
    {
        BinaryOperationMap = (left, right) => (operandTypes.Contains(left) && right == left) ? returnType : null;
        return this;

    }
    public Operator AddAddressOperations()
    {
        UnaryOperationMap = (operand) => new PointerTypeSymbol(operand);

        return this;
    }
    public Operator AddDereferenceOperations()
    {
        UnaryOperationMap = (operand) => (operand is PointerTypeSymbol ptr) ? ptr.Type : null;

        return this;
    }
    public Operator AddUnaryPointerOperations()
    {
        var prev = UnaryOperationMap;
        UnaryOperationMap = (operand) => operand is PointerTypeSymbol ? operand : prev?.Invoke(operand);

        return this;
    }
    public Operator AddBinaryPointerIntegerOperations()
    {
        var prev = BinaryOperationMap;
        BinaryOperationMap = (left, right) =>
        {
            if (left is PointerTypeSymbol && Types.IntegerTypes.Contains(right))
            {
                return left;
            }

            return prev?.Invoke(left, right);
        };

        return this;
    }

    public Operator AddStringOperations()
    {

        var prev = BinaryOperationMap;
        BinaryOperationMap = (left, right) =>
        {
            if (left.Name == "string")
            {
                return left;
            }
            if (right.Name == "string")
            {
                return left;
            }

            return prev?.Invoke(left, right);
        };

        return this;
    }

    public TypeSymbol? GetUnaryOperationReturnType(TypeSymbol operand) => UnaryOperationMap?.Invoke(operand);
    public TypeSymbol? GetBinaryOperationReturnType(TypeSymbol left, TypeSymbol right)
    {
        var res = BinaryOperationMap?.Invoke(left, right);

        if (res != null)
            return res;

        var common = Types.GetCommonTypeSymbol(left, right);
        return common == null ? null : BinaryOperationMap?.Invoke(common, common);
    }


    public static readonly Operator[] AllOperators =
    [
        new Operator("++", PostIncrement, 16, isAssigning: true).AddUnaryOperations(Types.IntegerTypes).AddUnaryPointerOperations(),
        new Operator("--", PostDecrement, 16, isAssigning: true).AddUnaryOperations(Types.IntegerTypes).AddUnaryPointerOperations(),

        new Operator("++", PreIncrement, 15, Associativity.Right, isAssigning: true).AddUnaryOperations(Types.IntegerTypes).AddUnaryPointerOperations(),
        new Operator("--", PreDecrement, 15, Associativity.Right, isAssigning: true).AddUnaryOperations(Types.IntegerTypes).AddUnaryPointerOperations(),
        new Operator("-", Neg, 15, Associativity.Right).AddUnaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),
        new Operator("!", LogicalNot, 15, Associativity.Right).AddUnaryOperation(Types.Bool),
        new Operator("~", BitwiseNot, 15, Associativity.Right).AddUnaryOperations(Types.IntegerTypes),

        new Operator("&", Address, 15, Associativity.Right).AddAddressOperations(),
        new Operator("*", Dereference, 15, Associativity.Right).AddDereferenceOperations(),

        new Operator("*", Mul, 12).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),
        new Operator("/", Div, 12).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),
        new Operator("%", Mod, 12).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),

        new Operator("+", Add, 11).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]).AddBinaryPointerIntegerOperations().AddStringOperations(),
        new Operator("-", Sub, 11).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]).AddBinaryPointerIntegerOperations(),

        new Operator("<<", ShiftLeft, 10).AddBinaryOperations(Types.IntegerTypes),
        new Operator(">>", ShiftRight, 10).AddBinaryOperations(Types.IntegerTypes),

        new Operator("<", Less, 7).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes], Types.Bool),
        new Operator("<=", LessOrEqual, 7).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes], Types.Bool),
        new Operator(">", Greater, 7).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes], Types.Bool),
        new Operator(">=", GreaterOrEqual, 7).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes], Types.Bool),



        new Operator("==", Equal, 6).AddBinaryOperations([.. Types.IntegerTypes, .. Types.FloatingPointTypes, Types.Bool], Types.Bool),
        new Operator("!=", NotEqual, 6).AddBinaryOperations([.. Types.IntegerTypes, .. Types.FloatingPointTypes, Types.Bool], Types.Bool),

        new Operator("&", BitwiseAnd, 5).AddBinaryOperation(Types.Int),

        new Operator("^", BitwiseXor, 4).AddBinaryOperation(Types.Int),

        new Operator("|", BitwiseOr, 3).AddBinaryOperation(Types.Int),

        new Operator("&&", LogicalAnd, 2).AddBinaryOperation(Types.Bool),

        new Operator("||", LogicalOr, 1).AddBinaryOperation(Types.Bool),


        new Operator("=", Assign, 0, isAssigning: true).AddBinaryOperations([]),
        new Operator("+=", AddAssign, 0, isAssigning: true).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]).AddBinaryPointerIntegerOperations(),
        new Operator("-=", SubAssign, 0, isAssigning: true).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]).AddBinaryPointerIntegerOperations(),
        new Operator("*=", MulAssign, 0, isAssigning: true).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),
        new Operator("/=", DivAssign, 0, isAssigning: true).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),
        new Operator("%=", ModAssign, 0, isAssigning: true).AddBinaryOperations([..Types.IntegerTypes, ..Types.FloatingPointTypes]),
    ];                                 


    public static Operator? GetOperator(string symbol, bool unary = false)
    {


        return AllOperators.FirstOrDefault(x =>
            x.Symbol == symbol
            && (unary ? x.UnaryOperationMap != null : x.BinaryOperationMap != null)
            );
    }

    public static Operator? GetUnaryOperator(string symbol, bool prefix)
    {


        return AllOperators.FirstOrDefault(x =>
            x.Symbol == symbol
            && (x.UnaryOperationMap != null)
            && (x.Associativity == Associativity.Right == prefix)
        );
    }
}