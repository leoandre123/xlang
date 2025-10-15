using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static System.Runtime.InteropServices.JavaScript.JSType;
using String = System.String;


namespace xlang.Compiler.Structures.AST
{
    public record Node(SourceSpan Span)
    {
       // public required SourceSpan Span { get; init;}
        public int Id { get; } = GenerateId();

        private static int _currentId = 0;
        private static int GenerateId()
        {
            return _currentId++;
        }
    }

    //STATEMENTS
    public record Statement(SourceSpan Span) : Node(Span);

    public record VariableDeclaration(string Name, string Type, Expression? Initial, SourceSpan Span) : Statement(Span);
    public record ArrayDeclaration(string Name, string Type, Expression Size, Expression? Initial, SourceSpan Span) : VariableDeclaration(Name, Type, Initial, Span);
    public record IfStatement(Expression Expression, Statement Body, Statement? Else, SourceSpan Span) : Statement(Span);
    public record WhileStatement(Expression Expression, Statement Body, SourceSpan Span) : Statement(Span);
    public record ForStatement(Statement Initial, Expression Expression, Statement Increment, Statement Body, SourceSpan Span) : Statement(Span);
    public record Return(Expression? Expression, SourceSpan Span) : Statement(Span);
    public record Break(SourceSpan Span) : Statement(Span);
    public record Block(IReadOnlyList<Statement> Statements, SourceSpan Span) : Statement(Span);

    /*
     * Expressions
     */
    public record Expression(SourceSpan Span) : Statement(Span);

    public record IntegerLiteralExpression(int Value, SourceSpan Span) : Expression(Span);
    public record FloatLiteralExpression(float Value, SourceSpan Span) : Expression(Span);
    public record BooleanLiteralExpression(bool Value, SourceSpan Span) : Expression(Span);
    public record StringLiteralExpression(string Value, SourceSpan Span) : Expression(Span);
    public record VariableExpression(string Name, SourceSpan Span) : Expression(Span);
    public record CallExpression(string Name, IReadOnlyList<Expression> Arguments, SourceSpan Span) : Expression(Span);
    public record IndexExpression(Expression Base, Expression Index, SourceSpan Span) : Expression(Span);
    public record StringInterpolationExpression(IReadOnlyList<Expression> Parts, SourceSpan Span) : Expression(Span);
    public record MemberAccessExpression(Expression Base, string Member, int Count, SourceSpan Span) : Expression(Span);
    public record MemberCallExpression(Expression Base, string Member, IReadOnlyList<Expression> Arguments, SourceSpan Span) : Expression(Span);
    public record ArrayInitializerListExpression(IReadOnlyList<Expression> Elements, SourceSpan Span) : Expression(Span);
    public record MakeExpression(Expression Call, SourceSpan Span) : Expression(Span);

    public record UnaryExpression(Operator Operator, Expression Operand, SourceSpan Span) : Expression(Span);
    public record BinaryExpression(Expression Left, Operator Operator, Expression Right, SourceSpan Span) : Expression(Span);

    public record AliasDeclaration(string Name, string Type, SourceSpan Span) : Node(Span);

    public record ScopeBody(
        IReadOnlyList<FunctionDeclaration> Functions,
        IReadOnlyList<ScopeDeclaration> Scopes,
        IReadOnlyList<AliasDeclaration> Aliases,
        IReadOnlyList<ClassDeclaration> Classes,
        SourceSpan Span) : Node(Span);
    public record ScopeDeclaration(string Name, ScopeBody Body, SourceSpan Span) : Node(Span);
    public record FunctionParameter(string Name, string Type, SourceSpan Span) : Node(Span);
    public record FunctionDeclaration(
        string Name,
        string ReturnType,
        EAccessType AccessType,
        IReadOnlyList<FunctionParameter> Parameters,
        bool IsInstance,
        bool IsExtern,
        bool IsUnsafe,
        Block? Body,
        SourceSpan Span) : Node(Span);

    //public record ClassMethodDeclaration(string Name, string ReturnType, IReadOnlyList<FunctionParameter> Parameters, Block Body, SourceSpan Span) : FunctionDeclaration(Name, ReturnType, EAccessType.Private, Parameters, Body, Span);
    //public record ClassConstructor(IReadOnlyList<FunctionParameter> Parameters, Block Body, SourceSpan Span) : ClassMember(Span);
    public record ClassField(string Name, string Type, EAccessType AccessType, Expression? Initial, bool IsInline, SourceSpan Span) : Node(Span);
    public record ClassDeclaration(string Name, IReadOnlyList<Node> Members, SourceSpan Span) : Node(Span);

    public record ProgramDeclaration(IReadOnlyList<ProgramImport> Imports, ScopeBody Body, SourceSpan Span) : Node(Span);

    public record ProgramImport(string Scope, SourceSpan Span) : Node(Span);
    public record FunctionAttribute(string Name, IReadOnlyList<Expression> Arguments, SourceSpan Span) : Node(Span);

    public record GlobalVariableDeclaration(
        string Name,
        string Type,
        EAccessType AccessType,
        Expression? Initial,
        bool IsConstant,
        SourceSpan Span) : Node(Span);

    public record Module(SourceSpan Span) : Node(Span)
    {
        public required string File { get; init; }
        public required ProgramDeclaration Program { get; init; }
    }


    public static class NodeHelper
    {
        public static bool IsNumericLiteral(this Expression expression) =>
             expression switch
             {
                 IntegerLiteralExpression or BooleanLiteralExpression => true,
                 _ => false
             };

        public static int GetNumericalLiteralValue(this Expression expression) =>

            expression switch
            {
                IntegerLiteralExpression intLit => intLit.Value,
                BooleanLiteralExpression boolLit => boolLit.Value ? 1 : 0,
                _ => throw new NotSupportedException($"Cannot resolve the numeric literal value of '{expression.GetType()}'")
            };

    }


    public abstract class NodeRewriter
    {
        public virtual ProgramDeclaration RewriteProgramDeclaration(ProgramDeclaration program)
        {
            return program;
            //var functions = RewriteList(program.Functions, RewriteFunction);
            //
            //
            //return ReferenceEquals(functions, program.Functions)
            //    ? program
            //    : program with { Functions = functions };

        }
        protected virtual FunctionDeclaration RewriteFunction(FunctionDeclaration function)
        {
            var block = RewriteBlock(function.Body);


            return ReferenceEquals(block, function.Body)
                ? function
                : function with { Body = block };
        }
        protected virtual Block RewriteBlock(Block block)
        {
            var statements = RewriteList(block.Statements, RewriteStatement);


            return ReferenceEquals(statements, block.Statements)
                ? block
                : block with { Statements = statements };
        }
        protected virtual Statement RewriteStatement(Statement statement)
        {
            return statement switch
            {
                Block block => RewriteBlock(block),
                Return ret => RewriteReturn(ret),
                VariableDeclaration varDec => RewriteVariableDeclaration(varDec),
                IfStatement ifStatement => RewriteIfStatment(ifStatement),
                WhileStatement whileStatement => RewriteWhileStatement(whileStatement),
                ForStatement forStatement => RewriteForStatement(forStatement),
                Expression expression => RewriteExpression(expression),

                _ => throw new NotSupportedException($"Unhandled type '{statement.GetType()}'")
            };
        }
        protected virtual ForStatement RewriteForStatement(ForStatement forStatement)
        {
            var expr = RewriteExpression(forStatement.Expression);
            var init = RewriteStatement(forStatement.Initial);
            var inc = RewriteStatement(forStatement.Increment);
            var body = RewriteStatement(forStatement.Body);

            return (ReferenceEquals(expr, forStatement.Expression)
                    && ReferenceEquals(body, forStatement.Body)
                    && ReferenceEquals(init, forStatement.Initial)
                    && ReferenceEquals(inc, forStatement.Increment))
                ? forStatement
                : forStatement with { Expression = expr, Body = body, Increment = inc, Initial = init };
        }
        protected virtual WhileStatement RewriteWhileStatement(WhileStatement whileStatement)
        {
            var expr = RewriteExpression(whileStatement.Expression);
            var body = RewriteStatement(whileStatement.Body);

            return (ReferenceEquals(expr, whileStatement.Expression) && ReferenceEquals(body, whileStatement.Body))
                ? whileStatement
                : whileStatement with { Expression = expr, Body = body };
        }
        protected virtual IfStatement RewriteIfStatment(IfStatement ifStatement)
        {
            var expr = RewriteExpression(ifStatement.Expression);
            var body = RewriteStatement(ifStatement.Body);
            var el = ifStatement.Else == null ? null : RewriteStatement(ifStatement.Else);

            return (ReferenceEquals(expr, ifStatement.Expression) && ReferenceEquals(body, ifStatement.Body) && ReferenceEquals(el, ifStatement.Else))
                ? ifStatement
                : ifStatement with { Expression = expr, Body = body, Else = el };
        }
        protected virtual VariableDeclaration RewriteVariableDeclaration(VariableDeclaration varDec)
        {
            if (varDec.Initial == null) return varDec;
            var expr = RewriteExpression(varDec.Initial);

            return ReferenceEquals(expr, varDec.Initial)
                ? varDec
                : varDec with { Initial = expr };
        }
        protected virtual Return RewriteReturn(Return ret)
        {
            if (ret.Expression == null) return ret;
            var expr = RewriteExpression(ret.Expression);

            return ReferenceEquals(expr, ret.Expression)
                ? ret
                : ret with { Expression = expr };

        }
        protected virtual Expression RewriteExpression(Expression expression)
        {
            return expression switch
            {
                IntegerLiteralExpression => expression,
                FloatLiteralExpression => expression,
                BooleanLiteralExpression => expression,
                StringLiteralExpression => expression,
                VariableExpression => expression,
                StringInterpolationExpression strIntExpr => RewriteStringInterpolation(strIntExpr),

                CallExpression callExpr => RewriteCall(callExpr),
                IndexExpression indExpr => RewriteIndex(indExpr),
                MemberAccessExpression accExpr => RewriteAccess(accExpr),
                MemberCallExpression memCallExpr => expression,

                BinaryExpression binaryExpr => RewriteBinary(binaryExpr),
                UnaryExpression unaryExpr => RewriteUnary(unaryExpr),
                ArrayInitializerListExpression => expression,

                _ => throw new NotSupportedException($"Unhandled type '{expression.GetType()}'")
            };
        }

        protected virtual Expression RewriteStringInterpolation(StringInterpolationExpression strIntExpr)
        {
            var parts = RewriteList(strIntExpr.Parts, RewriteExpression);

            return ReferenceEquals(parts, strIntExpr.Parts)
                ? strIntExpr
                : strIntExpr with { Parts = parts };
        }
        protected virtual Expression RewriteCall(CallExpression callExpr)
        {
            var arguments = RewriteList(callExpr.Arguments, RewriteExpression);

            return ReferenceEquals(arguments, callExpr.Arguments)
                ? callExpr
                : callExpr with { Arguments = arguments };

        }
        protected virtual Expression RewriteIndex(IndexExpression indExpr)
        {
            var b = RewriteExpression(indExpr.Base);
            var i = RewriteExpression(indExpr.Index);

            return (ReferenceEquals(b, indExpr.Base) && ReferenceEquals(i, indExpr.Index))
                ? indExpr
                : indExpr with { Base = b, Index = i };

        }
        protected virtual Expression RewriteAccess(MemberAccessExpression accExpr)
        {
            var b = RewriteExpression(accExpr.Base);

            return ReferenceEquals(b, accExpr.Base)
                ? accExpr
                : accExpr with { Base = b };

        }
        protected virtual Expression RewriteBinary(BinaryExpression binaryExpr)
        {
            var left = RewriteExpression(binaryExpr.Left);
            var right = RewriteExpression(binaryExpr.Right);

            return (ReferenceEquals(left, binaryExpr.Left) && ReferenceEquals(right, binaryExpr.Right))
                ? binaryExpr
                : binaryExpr with { Left = left, Right = right };

        }
        protected virtual Expression RewriteUnary(UnaryExpression unaryExpr)
        {
            var operand = RewriteExpression(unaryExpr.Operand);

            return ReferenceEquals(operand, unaryExpr.Operand)
                ? unaryExpr
                : unaryExpr with { Operand = operand };

        }
        protected virtual IReadOnlyList<T> RewriteList<T>(IReadOnlyList<T> list, Func<T, T> rewriteFunc)
        {
            List<T>? tmp = null;
            for (var i = 0; i < list.Count; i++)
            {
                var oldElement = list[i];
                var newElement = rewriteFunc(oldElement);
                if (!ReferenceEquals(newElement, oldElement))
                {
                    tmp ??= new(list);
                    tmp[i] = newElement;
                }
            }
            return tmp ?? list;
        }

    }
}

