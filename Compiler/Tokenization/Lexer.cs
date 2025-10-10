namespace xlang.Compiler.Tokenization;

public record Token
{
    public TokenType Type { get; }
    public SourceSpan Span { get; }
    public string? Value { get; } = null;
    //public OperatorType OperatorType { get; } = OperatorType.None;
    public bool IsCompoundAssignment { get; } = false;


    public Token(TokenType type, SourceSpan span)
    {
        Type = type;
        Span = span;
    }

    public Token(TokenType type, string value, SourceSpan span)
    {
        Type = type;
        Span = span;
        Value = value;
    }

    public Token(TokenType type, OperatorType operatorType, SourceSpan span)
    {
        Type = type;
        Span = span;
        //OperatorType = operatorType;
    }
    public Token(TokenType type, OperatorType operatorType, bool compoundAssignment, SourceSpan span)
    {
        Type = type;
        Span = span;
        //OperatorType = operatorType;
        IsCompoundAssignment = compoundAssignment;
    }

}

public class Lexer
{

    private static readonly Dictionary<string, TokenType> Keywords = new()
    {
        {"return", TokenType.Return},
        {"switch", TokenType.Switch},
        {"for", TokenType.For},
        {"while", TokenType.While},
        {"if", TokenType.If},
        {"else", TokenType.Else},
        {"break", TokenType.Break},

        //{"int", TokenType.IntType},
        //{"bool", TokenType.BoolType},
        //{"void", TokenType.VoidType},

        {"false", TokenType.BooleanLiteral},
        {"true", TokenType.BooleanLiteral},

        {"alias", TokenType.Type},
        {"extern", TokenType.Extern},
        {"global", TokenType.Global},
        {"class", TokenType.Class},
        {"import", TokenType.Import},
        {"scope", TokenType.Scope}
    };

    enum LexingMode
    {
        Default,
        InterpolatingText,
        InterpolatingExpression,
    }

    private readonly string _source;
    private readonly string _sourceFile;
    private int _currentIndex = 0;
    private LexingMode _mode = LexingMode.Default;
    private int _braceCount = 0;

    public Lexer(string source, string fileName)
    {
        _source = source;
        _sourceFile = fileName;
    }

    private bool Eof() => _currentIndex >= _source.Length;
    private char Peek(int forward = 0) => _source[_currentIndex + forward];
    private char Consume() => _source[_currentIndex++];
    private void SkipWhiteSpace()
    {
        while (!Eof() && char.IsWhiteSpace(Peek()))
        {
            Consume();
        }
    }


    public Token NextToken()
    {
        if (_mode == LexingMode.Default)
        {
            return Next();
        }
        else
        {
            return NextInterp();
        }

    }

    private Token NextInterp()
    {
        var startIndex = _currentIndex;
        if (Eof())
            return new Token(TokenType.Eof, new SourceSpan(startIndex, 0));
        var c = Consume();
        var currentValue = c.ToString();


        if (_mode == LexingMode.InterpolatingText)
        {
            if (c == '"')
            {
                _mode = LexingMode.Default;
                return new Token(TokenType.InterpolationEnd, new SourceSpan(startIndex, _currentIndex - startIndex));
            }
            while (!Eof())
            {
                if (Peek() == '{')
                {
                    _mode = LexingMode.InterpolatingExpression;
                    break;
                }
                if (Peek() == '"')
                {
                    break;
                }

                currentValue += Consume();
            }

            return new Token(TokenType.StringLiteral, currentValue, new SourceSpan(startIndex, _currentIndex - startIndex));
        }

        if (_mode == LexingMode.InterpolatingExpression)
        {
            var token = Next();
            if (token.Type == TokenType.LeftBrace)
            {
                _braceCount++;
            }
            else if (token.Type == TokenType.RightBrace)
            {
                _braceCount++;
            }

            if (_braceCount == 0)
            {
                _mode = LexingMode.InterpolatingText;
            }

            return token;
        }
        else
        {
            throw new Exception();
        }
    }
    private Token Next()
    {
        SkipWhiteSpace();


        var startIndex = _currentIndex;

        if (Eof())
            return new Token(TokenType.Eof, new SourceSpan(startIndex, 0));


        var c = Consume();
        var currentValue = c.ToString();

        if (c == '$')
        {
            _mode = LexingMode.InterpolatingText;
            Consume(); // Consume "
            return new Token(TokenType.InterpolationStart, new SourceSpan(startIndex, _currentIndex - startIndex));
        }

        //Numerical literal (5, 0.2, .4)
        if (char.IsDigit(c))
        {
            while (!Eof() && (char.IsDigit(Peek()) || Peek() == '.'))
            {
                currentValue += Consume();
            }

            if (Peek() == 'f')
            {
                currentValue += Consume();
            }

            return new Token(TokenType.IntegerLiteral, currentValue, new SourceSpan(startIndex, _currentIndex - startIndex));
        }

        if (c == '"')
        {
            currentValue = "";
            while (!Eof() && Peek() != '"')
            {
                currentValue += Consume();
            }
            Consume();
            return new Token(TokenType.StringLiteral, currentValue, new SourceSpan(startIndex, _currentIndex - startIndex));
        }


        //Keyword or identifier or 
        if (char.IsLetter(c) || c == '_')
        {
            while (!Eof() && (char.IsLetterOrDigit(Peek()) || Peek() == '_'))
            {
                currentValue += Consume();
            }

            return Keywords.ContainsKey(currentValue) ?
                new Token(Keywords[currentValue], currentValue, new SourceSpan(startIndex, _currentIndex - startIndex)) :
                new Token(TokenType.Identifier, currentValue, new SourceSpan(startIndex, _currentIndex - startIndex));
        }


        var n = Eof() ? '\0' : Peek();

        var sym1 = $"{c}{n}";
        var sym2 = $"{c}";

        if (sym1 == "//")
        {
            Consume();
            while (!Eof() && Peek() != '\n')
            {
                currentValue += Consume();
            }

            return Next();
        }
        if (sym1 == "/*")
        {
            Consume();
            
            while (!Eof() && !(Peek() == '*' && Peek(1) == '/'))
            {
                currentValue += Consume();
            }

            Consume();
            Consume();

            return Next();
        }

        var op = Operator.AllOperators.FirstOrDefault(x => x.Symbol == sym1) ?? Operator.AllOperators.FirstOrDefault(x => x.Symbol == sym2);
        if (op != null)
        {
            if (op.Symbol.Length == 2)
                Consume();
            return new Token(TokenType.Operator, op.Symbol, new SourceSpan(startIndex, _currentIndex - startIndex));
        }

        return c switch
        {
            '(' => new Token(TokenType.LeftParenthesis, new SourceSpan(startIndex, _currentIndex - startIndex)),
            ')' => new Token(TokenType.RightParenthesis, new SourceSpan(startIndex, _currentIndex - startIndex)),
            '{' => new Token(TokenType.LeftBrace, new SourceSpan(startIndex, _currentIndex - startIndex)),
            '}' => new Token(TokenType.RightBrace, new SourceSpan(startIndex, _currentIndex - startIndex)),
            '[' => new Token(TokenType.LeftBracket, new SourceSpan(startIndex, _currentIndex - startIndex)),
            ']' => new Token(TokenType.RightBracket, new SourceSpan(startIndex, _currentIndex - startIndex)),

            '@' => new Token(TokenType.At, new SourceSpan(startIndex, _currentIndex - startIndex)),

            ';' => new Token(TokenType.Semicolon, new SourceSpan(startIndex, _currentIndex - startIndex)),
            ',' => new Token(TokenType.Comma, new SourceSpan(startIndex, _currentIndex - startIndex)),
            '.' => new Token(TokenType.Period, new SourceSpan(startIndex, _currentIndex - startIndex)),
            _ => throw new LexingException($"Unexpected char '{c}'", new SourceSpan(startIndex, _currentIndex - startIndex), _sourceFile)
        };
    }

    public List<Token> Tokenize()
    {
        Logger.LogDebug("Tokenizing");
        Token currentToken;
        List<Token> tokens = [];

        while ((currentToken = NextToken()).Type != TokenType.Eof)
        {
            tokens.Add(currentToken);
        }
        tokens.Add(currentToken);

        return tokens;
    }
}
