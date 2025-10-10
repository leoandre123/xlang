using static xlang.Compiler.Tokenization.TokenType;

namespace xlang.Compiler.Tokenization;

public enum TokenType
{
    //Literals
    IntegerLiteral,
    FloatLiteral,
    BooleanLiteral,
    StringLiteral,


    //
    Global,
    Private,

    Type,
    Extern,
    Class,
    Scope,
    Import,

    //
    InterpolationStart,
    InterpolationEnd,

    Comment,

    //
    At,

    //
    Identifier,

    //Statements
    Return,
    Switch,
    For,
    While,
    If,
    Else,

    Break,
    Continue,


    //Operators
    Operator,


    //Parentheses
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    Semicolon,
    Comma,
    Period,

    Eof
}