using System.Globalization;
using xlang.Compiler.SemanticAnalysis;
using xlang.Compiler.Tokenization;
using xlang.Compiler.Structures.AST;
using System.Reflection.Metadata;
using System.Security.Cryptography.X509Certificates;
using xlang.Compiler.Structures;


namespace xlang.Compiler.Parsing;




public class Parser
{
    private readonly List<Token> _tokens;
    private readonly string _file;
    private int _currentIndex = 0;
    private readonly HashSet<string> _typeNames = [.. Types.BuiltIn.Select(x => x.Name)];


    public TokenType[] AccessModifiers = [TokenType.Public, TokenType.Private];
    //public TokenType[] Modifiers = [];


    public Parser(List<Token> tokens, string filePath)
    {
        _tokens = tokens;
        _file = filePath;
    }

    private bool IsType(TokenType type, int forward = 0) => _tokens[_currentIndex + forward].Type == type;
    private bool IsType(TokenType[] types, int forward = 0) => types.Contains(_tokens[_currentIndex + forward].Type);
    private Token Peek(int forward = 0) => _tokens[_currentIndex + forward];
    private Token Consume() => _tokens[_currentIndex++];
    private Token Expect(TokenType type, string? message = null) => Peek().Type == type ?
        _tokens[_currentIndex++] :
        throw new ParsingException(message ?? $"Expected {type} but found {Peek().Type}", Peek().Span, _file);
    private bool IsTypeAndConsume(TokenType type)
    {
        if (!IsType(type)) return false;
        Consume();
        return true;

    }
    private bool IsBinary() =>
        _tokens[_currentIndex].Type == TokenType.Operator &&
        Operator.GetOperator(_tokens[_currentIndex].Value ?? "") != null;

    private EAccessType ParseAccessType()
    {
        var accMod = Consume();
        return accMod.Type switch
        {
            TokenType.Public => EAccessType.Public,
            TokenType.Module => EAccessType.Module,
            TokenType.Private => EAccessType.Private,
            _ => throw new NotImplementedException(),
        };
    }



    private BooleanLiteralExpression ParseBooleanLiteral()
    {
        var lit = Expect(TokenType.BooleanLiteral);
        if (bool.TryParse(lit.Value, out var val))
        {
            return new BooleanLiteralExpression(val, lit.Span);
        }

        throw new ParsingException("Could not parse boolean literal", lit.Span, _file);
    }
    private Expression ParseNumericLiteral()
    {
        var lit = Expect(TokenType.NumericLiteral);

        if (int.TryParse(lit.Value, out var i))
        {
            return new IntegerLiteralExpression(i, lit.Span);
        }
        if (float.TryParse(lit.Value!.TrimEnd('f'), CultureInfo.InvariantCulture, out var f))
        {
            return new FloatLiteralExpression(f, lit.Span);
        }

        throw new ParsingException($"Could not parse literal '{lit.Value}'", lit.Span, _file);
    }

    private Expression ParseExpression(int minPrecedence = 0)
    {
        // x++


        //var lhs = ParseUnaryOrPrimary();

        var lhs = ParsePrefix();
        lhs = ParsePostfix(lhs);

        while (IsBinary() && Operator.GetOperator(Peek().Value).Precedence >= minPrecedence)
        {
            var opTok = Consume();
            var op = Operator.GetOperator(opTok.Value!)!;
            var nextMin = op.Associativity == Associativity.Left ? op.Precedence + 1 : op.Precedence;
            //var rhs = ParseExpression(nextMin);

            //var rhs = ParsePrefix();
            //rhs = ParsePostfix(rhs);

            var rhs = ParseExpression(nextMin);

            lhs = new BinaryExpression(lhs, op, rhs, SourceSpan.FromStartAndEnd(lhs.Span.Start, rhs.Span.End));
        }

        return lhs;
    }

    private Expression ParseUnaryOrPrimary()
    {
        Operator? op;
        if (IsType(TokenType.Operator) && (op = Operator.GetOperator(Peek().Value, true)) != null)
        {
            var opTok = Consume();
            var exp = ParseUnaryOrPrimary();
            return new UnaryExpression(op, exp, SourceSpan.FromStartAndEnd(opTok.Span.Start, exp.Span.End));
        }
        if (IsType(TokenType.Identifier))
        {
            if (Peek(1).Type == TokenType.Operator &&
                (op = Operator.GetOperator(Peek(1).Value, true)) != null)
            {
                var identToken = Consume();
                var exp = new VariableExpression(identToken.Value, identToken.Span);
                var opTok = Consume();
                return new UnaryExpression(op, exp, SourceSpan.FromStartAndEnd(identToken.Span.Start, opTok.Span.End));
            }
        }

        return ParsePrimaryExpression();
    }


    private Expression ParsePrefix()
    {

        if (IsType(TokenType.Operator))
        {
            Operator? op;
            if ((op = Operator.GetUnaryOperator(Peek().Value, true)) != null)
            {
                var opTok = Consume();
                var exp = ParseUnaryOrPrimary();
                return new UnaryExpression(op, exp, SourceSpan.FromStartAndEnd(opTok.Span.Start, exp.Span.End));
            }

            throw new ParsingException($"Unknown prefix operator '{Peek().Value}'", Peek().Span, _file);
        }

        return ParsePrimaryExpression();
    }


    private Expression ParsePostfix(Expression expression)
    {
        if (IsType(TokenType.Operator))
        {
            Operator? op;
            if ((op = Operator.GetUnaryOperator(Peek().Value, false)) != null)
            {
                var opTok = Consume();
                expression = new UnaryExpression(op, expression, SourceSpan.FromStartAndEnd(expression.Span.Start, opTok.Span.End));
                return ParsePostfix(expression);
            }

            return expression;
        }

        //Function call
        if (IsType(TokenType.LeftParenthesis))
        {
            var lp = Consume();

            var args = new List<Expression>();
            while (!IsType(TokenType.RightParenthesis))
            {
                if (args.Count != 0)
                    Expect(TokenType.Comma);

                args.Add(ParseExpression());
            }
            var rp = Consume();

            if (expression is VariableExpression ve)
            {
                expression = new CallExpression(ve.Name, args,
                    SourceSpan.FromStartAndEnd(expression.Span.Start, rp.Span.End));

                return ParsePostfix(expression);
            }

            throw new ParsingException("Only simple calls by name supported for now", expression.Span, _file);
        }

        //Indexing
        if (IsType(TokenType.LeftBracket))
        {
            var lb = Consume();
            var indexExpr = ParseExpression();
            var rb = Expect(TokenType.RightBracket);

            expression = new IndexExpression(expression, indexExpr,
                SourceSpan.FromStartAndEnd(expression.Span.Start, rb.Span.End));

            return ParsePostfix(expression);
        }


        if (IsType(TokenType.Period))
        {
            var dot = Consume();
            var count = 1;
            while (IsTypeAndConsume(TokenType.Period))
                count++;

            var member = Expect(TokenType.Identifier);

            if (IsType(TokenType.LeftParenthesis))
            {
                var lp = Consume();

                var args = new List<Expression>();
                while (!IsType(TokenType.RightParenthesis))
                {
                    if (args.Count != 0)
                        Expect(TokenType.Comma);

                    args.Add(ParseExpression());
                }
                var rp = Consume();

                expression = new MemberCallExpression(expression, member.Value!, args,
                    SourceSpan.FromStartAndEnd(expression.Span.Start, rp.Span.End));
            }
            else
            {
                expression = new MemberAccessExpression(expression, member.Value!, count,
                    SourceSpan.FromStartAndEnd(expression.Span.Start, member.Span.End));
            }



            return ParsePostfix(expression);
        }

        return expression;
    }


    private Expression ParsePrimaryExpression()
    {
        if (IsType(TokenType.NumericLiteral))
        {
            return ParseNumericLiteral();
        }
        if (IsType(TokenType.BooleanLiteral))
        {
            return ParseBooleanLiteral();
        }
        if (IsType(TokenType.StringLiteral))
        {
            var token = Consume();
            return new StringLiteralExpression(token.Value!, token.Span);
        }
        if (IsType(TokenType.InterpolationStart))
        {
            var token = Consume();
            var literal = Expect(TokenType.StringLiteral);

            List<Expression> parts = [];

            while (!IsType(TokenType.InterpolationEnd))
            {
                if (IsType(TokenType.LeftBrace))
                {
                    Consume();
                    parts.Add(ParseExpression());
                    Expect(TokenType.RightBrace);
                }
                parts.Add(ParseExpression());
            }

            var endToken = Consume();

            return new StringInterpolationExpression(parts, SourceSpan.FromStartAndEnd(token.Span.Start, endToken.Span.End));
        }
        if (IsType(TokenType.Identifier))
        {
            var token = Consume();
            return new VariableExpression(token.Value!, token.Span);
        }
        if (IsType(TokenType.LeftParenthesis))
        {
            Consume();
            var expr = ParseExpression();
            Expect(TokenType.RightParenthesis);

            return expr;
        }
        if (IsType(TokenType.LeftBracket))
        {
            var startToken = Consume();
            List<Expression> elements = [];
            while (!IsType(TokenType.RightBracket))
            {
                if (elements.Count > 0)
                    Expect(TokenType.Comma);
                elements.Add(ParseExpression());
            }

            var endToken = Consume();

            return new ArrayInitializerListExpression(elements, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
        }
        if (IsType(TokenType.Make))
        {
            var startToken = Consume();
            var expr = ParseExpression();
            return new MakeExpression(expr, SourceSpan.FromStartAndEnd(startToken.Span.Start, expr.Span.End));
        }
        throw new ParsingException($"Could not parse primary expression '{Peek().Value}' with type '{Peek().Type}'", Peek().Span, _file);
    }

    private Return ParseReturn()
    {
        var returnToken = Expect(TokenType.Return);
        if (IsType(TokenType.Semicolon))
        {
            var token = Consume();
            return new Return(null, SourceSpan.FromStartAndEnd(returnToken.Span.Start, token.Span.End));
        }

        var returnExpression = ParseExpression();
        var endToken = Expect(TokenType.Semicolon);

        return new Return(returnExpression, SourceSpan.FromStartAndEnd(returnToken.Span.Start, endToken.Span.End));
    }
    private VariableDeclaration ParseVariableDeclaration()
    {
        var varType = Expect(TokenType.Identifier, "Expected a type");
        var typeName = varType.Value!;
        if (Peek().Value == "*")
        {
            Consume();
            typeName += "*";
        }
        var isArray = false;
        Expression arraySize = null;

        if (IsType(TokenType.LeftBracket))
        {
            isArray = true;
            Consume();
            if (!IsType(TokenType.RightBracket))
            {
                arraySize = ParseExpression();
            }
            Expect(TokenType.RightBracket);
        }

        var varName = Expect(TokenType.Identifier, "Expected a variable name");


        Expression? exp = null;

        if (IsType(TokenType.Operator) && Peek().Value == "=")
        {
            Consume();
            exp = ParseExpression();
        }

        var endToken = Expect(TokenType.Semicolon);

        if (isArray)
        {
            if (arraySize == null)
            {
                if (exp is ArrayInitializerListExpression aile)
                {
                    arraySize = new IntegerLiteralExpression(aile.Elements.Count, aile.Span);
                }
                else
                {
                    throw new ParsingException("Implicitly sized arrays must be initialized with list initializer ", SourceSpan.FromStartAndEnd(varType.Span.Start, endToken.Span.End), _file);
                }
            }


            return new ArrayDeclaration(varName.Value!, typeName, arraySize, exp,
                SourceSpan.FromStartAndEnd(varType.Span.Start, endToken.Span.End));
        }
        else
        {
            return new VariableDeclaration(varName.Value!, typeName, exp, SourceSpan.FromStartAndEnd(varType.Span.Start, endToken.Span.End));
        }
    }

    private IfStatement ParseIf()
    {
        var startToken = Expect(TokenType.If);
        Expect(TokenType.LeftParenthesis);
        var exp = ParseExpression();
        Expect(TokenType.RightParenthesis);
        var body = ParseStatement();
        Statement? elseStatement = null;
        if (IsType(TokenType.Else))
        {
            Consume(); //Consume else
            elseStatement = ParseStatement();
        }


        return new IfStatement(exp, body, elseStatement,
            SourceSpan.FromStartAndEnd(startToken.Span.Start, (elseStatement ?? body).Span.End));
    }
    private WhileStatement ParseWhile()
    {
        var startToken = Expect(TokenType.While);
        Expect(TokenType.LeftParenthesis);
        var exp = ParseExpression();
        Expect(TokenType.RightParenthesis);
        var body = ParseStatement();

        return new WhileStatement(exp, body,
            SourceSpan.FromStartAndEnd(startToken.Span.Start, body.Span.End));
    }
    private ForStatement ParseFor()
    {
        var startToken = Expect(TokenType.For);
        Expect(TokenType.LeftParenthesis);
        var init = ParseStatement();
        var exp = ParseExpression();
        Expect(TokenType.Semicolon);
        var inc = ParseStatement();
        Expect(TokenType.RightParenthesis);
        var body = ParseStatement();


        return new ForStatement(init, exp, inc, body,
            SourceSpan.FromStartAndEnd(startToken.Span.Start, body.Span.End));
    }
    private Statement ParseStatement()
    {
        if (IsType(TokenType.LeftBrace))
            return ParseBlock();
        if (IsType(TokenType.Return))
            return ParseReturn();
        if (_typeNames.Contains(Peek().Value!))
            return ParseVariableDeclaration();
        if (IsType(TokenType.Identifier))
        {
            if (Peek(1).Type == TokenType.Identifier)
            {
                return ParseVariableDeclaration();
            }
            var exp = ParseExpression();
            Expect(TokenType.Semicolon);
            return exp;
        }
        if (IsType(TokenType.If))
            return ParseIf();
        if (IsType(TokenType.While))
            return ParseWhile();
        if (IsType(TokenType.For))
            return ParseFor();
        if (IsType(TokenType.Break))
        {
            var br = Consume();
            Expect(TokenType.Semicolon);
            return new Break(br.Span);
        }


        throw new ParsingException($"Could not parse statement {Peek().Type}", Peek().Span, _file);
    }
    private Block ParseBlock()
    {
        var startToken = Expect(TokenType.LeftBrace);

        List<Statement> statements = [];

        while (!IsType(TokenType.RightBrace))
        {
            statements.Add(ParseStatement());
        }

        var endToken = Consume();
        return new Block(statements, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
    }


    private ClassField ParseClassVariable()
    {
        var accessType = IsType(AccessModifiers) ? ParseAccessType() : EAccessType.Private;
        var isInline = IsType(TokenType.Inline);
        if (isInline) Consume();

        var varDec = ParseVariableDeclaration();

        return new ClassField(varDec.Name, varDec.Type, accessType, varDec.Initial, isInline, varDec.Span);
    }

    //private ClassMethodDeclaration ParseClassMethod()
    //{
    //    var func = ParseFunction();
    //    return new ClassMethodDeclaration(func.Name, func.ReturnType, func.Parameters, func.Body, func.Span);
    //}

    //private ClassConstructor ParseConstructor()
    //{
    //    var startToken = Expect(TokenType.Identifier);
    //    Expect(TokenType.LeftParenthesis);
    //
    //    List<FunctionParameter> parameters = [];
    //
    //    while (!IsType(TokenType.RightParenthesis))
    //    {
    //        if (parameters.Count != 0)
    //        {
    //            Expect(TokenType.Comma);
    //        }
    //        var paramType = Expect(TokenType.Identifier, "Expected a type");
    //        var typeName = paramType.Value!;
    //        if (Peek().Value == "*")
    //        {
    //            Consume();
    //            typeName += "*";
    //        }
    //        var paramName = Expect(TokenType.Identifier, "Expected a param name");
    //
    //        parameters.Add(new FunctionParameter(paramName.Value, typeName,
    //            SourceSpan.FromStartAndEnd(paramType.Span.Start, paramName.Span.End)));
    //    }
    //    Expect(TokenType.RightParenthesis);
    //
    //    var body = ParseBlock();
    //
    //    return new ClassConstructor(parameters, body, SourceSpan.FromStartAndEnd(startToken.Span.Start, body.Span.End));
    //}
    private ClassDeclaration ParseClass()
    {
        var startToken = Expect(TokenType.Class);
        var nameToken = Expect(TokenType.Identifier);
        Expect(TokenType.LeftBrace);


        List<Node> members = [];

        while (!IsType(TokenType.RightBrace))
        {
            FunctionAttribute? att = null;

            if (IsType(TokenType.At))
                att = ParseAttribute();

            var i = 0;
            if (IsType(AccessModifiers, i)) i++;
            if (IsType(TokenType.LeftParenthesis, i + 1)) members.Add(ParseFunction(true, true));
            else if (IsType(TokenType.LeftParenthesis, i + 2)) members.Add(ParseFunction(true, false));
            else members.Add(ParseClassVariable());


        }

        var endToken = Consume();
        return new ClassDeclaration(nameToken.Value!, members, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
    }

    private FunctionAttribute ParseAttribute()
    {
        var startToken = Expect(TokenType.At);
        var attributeType = Expect(TokenType.Identifier);

        var endToken = attributeType;

        List<Expression> arguments = [];
        if (IsType(TokenType.LeftParenthesis))
        {
            Consume();
            while (!IsType(TokenType.RightParenthesis))
            {
                if (arguments.Count != 0)
                {
                    Expect(TokenType.Comma);
                }

                arguments.Add(ParseExpression());
            }

            endToken = Consume(); //Consume right par
        }

        // = Expect(TokenType.Semicolon);

        return new FunctionAttribute(attributeType.Value, arguments, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
    }
    private AliasDeclaration ParseType()
    {
        var startToken = Expect(TokenType.Alias);
        var ident = Expect(TokenType.Identifier);
        Expect(TokenType.Operator);
        var type = Expect(TokenType.Identifier);


        var postfix = "";
        if (IsType(TokenType.Operator) && Peek().Value == "*")
        {
            postfix = "*";
            Consume();
        }

        var endToken = Expect(TokenType.Semicolon);

        _typeNames.Add(ident.Value!);

        return new AliasDeclaration(ident.Value!, type.Value + postfix, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
    }
    private FunctionDeclaration ParseFunction(bool isInstance, bool isConstructor, FunctionAttribute? attribute = null)
    {
        Token startToken = Peek();

        var accessType = IsType(AccessModifiers) ? ParseAccessType(): EAccessType.Private;
        var isExtern = IsTypeAndConsume(TokenType.Extern);

        string returnType;
        if (isConstructor)
        {
            returnType = "";
        }
        else
        {
            var tok = Expect(TokenType.Identifier, "Expected a return type!");
            returnType = tok.Value!;

            if (Peek().Value == "*")
            {
                Consume();
                returnType += "*";
            }
        }

        var nameToken = Expect(TokenType.Identifier, "Expected a function name");
        Expect(TokenType.LeftParenthesis);

        List<FunctionParameter> parameters = [];

        while (!IsType(TokenType.RightParenthesis))
        {
            if (parameters.Count != 0)
            {
                Expect(TokenType.Comma);
            }
            var paramType = Expect(TokenType.Identifier, "Expected a type");
            var typeName = paramType.Value!;
            if (Peek().Value == "*")
            {
                Consume();
                typeName += "*";
            }
            var paramName = Expect(TokenType.Identifier, "Expected a param name");

            parameters.Add(new FunctionParameter(paramName.Value, typeName,
                SourceSpan.FromStartAndEnd(paramType.Span.Start, paramName.Span.End)));
        }
        Expect(TokenType.RightParenthesis);

        int spanEnd;
        Block? body = null;
        if (isExtern)
        {
            var endToken = Expect(TokenType.Semicolon);
            spanEnd = endToken.Span.End;
        }
        else
        {
            body = ParseBlock();
            spanEnd = body.Span.End;
        }

        var isUnsafe = attribute?.Name == "unsafe";

        return new FunctionDeclaration(nameToken.Value!, returnType, accessType, parameters, isInstance, isExtern, isUnsafe, body, SourceSpan.FromStartAndEnd(startToken.Span.Start, spanEnd));
    }

    private GlobalVariableDeclaration ParseGlobalVariableDeclaration()
    {
        var startToken = Peek();

        var isConst = IsTypeAndConsume(TokenType.Const);
        var accessType = IsType(AccessModifiers) ? ParseAccessType() : EAccessType.Private;

        var type = Expect(TokenType.Identifier).Value!;
        var name = Expect(TokenType.Identifier).Value!;

        Expression? expr = null;
        if (Peek() is { Type: TokenType.Operator, Value: "=" })
        {
            Consume();
            expr = ParseExpression();
        }

        var endToken = Expect(TokenType.Semicolon);

        return new GlobalVariableDeclaration(name, type, accessType, expr,
            isConst, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
    }

    private ScopeBody ParseScopeBody()
    {
        List<ClassDeclaration> classes = [];
        List<FunctionDeclaration> functions = [];
        List<AliasDeclaration> types = [];
        List<ScopeDeclaration> scopes = [];
        List<GlobalVariableDeclaration> globals = [];

        var firstToken = Peek();



        while (!IsType([TokenType.Eof, TokenType.RightBrace]))
        {
            FunctionAttribute? att = null;

            if (IsType(TokenType.At))
                att = ParseAttribute();

            if (IsType([TokenType.Const]))
                globals.Add(ParseGlobalVariableDeclaration());
            else if (IsType(TokenType.Class))
                classes.Add(ParseClass());
            else if (IsType([TokenType.Identifier, TokenType.Extern, ..AccessModifiers]))
                functions.Add(ParseFunction(false, false, att));
            else if (IsType(TokenType.Alias))
                types.Add(ParseType());
            else if (IsType(TokenType.Scope))
                scopes.Add(ParseScope());
            else
                throw new ParsingException("Only functions, classes and type declarations are allowed in the global scope", Peek().Span, _file);
        }

        return new ScopeBody(functions, scopes, types, classes,
            SourceSpan.FromStartAndEnd(firstToken.Span.Start, Peek().Span.End));
    }
    private ScopeDeclaration ParseScope()
    {
        var startToken = Expect(TokenType.Scope);
        var scope = Expect(TokenType.Identifier).Value!;
        while (IsType(TokenType.Period))
        {
            Consume();
            scope += "." + Expect(TokenType.Identifier).Value!;
        }

        Expect(TokenType.LeftBrace);
        var body = ParseScopeBody();
        var endToken = Expect(TokenType.RightBrace);
        return new ScopeDeclaration(scope, body,
            SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End));
    }

    public Module ParseProgram()
    {
        Logger.LogDebug("Parsing tokens");

        List<ProgramImport> imports = [];
        while (IsType(TokenType.Import))
        {
            var startToken = Consume();
            var scope = Expect(TokenType.Identifier).Value!;
            while (IsType(TokenType.Period))
            {
                Consume();
                scope += "." + Expect(TokenType.Identifier).Value;
            }
            var endToken = Expect(TokenType.Semicolon);

            imports.Add(new ProgramImport(scope, SourceSpan.FromStartAndEnd(startToken.Span.Start, endToken.Span.End)));
        }

        var body = ParseScopeBody();

        var program = new ProgramDeclaration(imports, body, body.Span);

        return new Module(body.Span)
        {
            File = _file,
            Program = program
        };
    }
}

