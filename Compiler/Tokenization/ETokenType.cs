using static xlang.Compiler.Tokenization.TokenType;

namespace xlang.Compiler.Tokenization;

public enum TokenType
{
    /* LITERALS */
    NumericLiteral,
    BooleanLiteral,
    StringLiteral,


    /* MODIFIERS */
    Public,
    Module,
    Private,

    Global,
    Extern,
    Inline,
    Const,

    Weak,
    Strong,

    /*  */
    Import,
    Scope,
    Alias,
    Class,
    Struct,
    Enum,

    //
    InterpolationStart,
    InterpolationEnd,

    
    //
    Make,


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