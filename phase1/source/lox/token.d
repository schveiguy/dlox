module lox.token;

import std.sumtype;

struct LoxCall
{
    private import lox.ast : Stmt;
    const(char)[] name;
    const(char[])[] parameters;
    Stmt[] statements;
    Value function(Value[]) nativeFn;
    string toString() {
        return (nativeFn ? "<nativeFn " : "<fn ") ~ cast(string)name ~ ">";
    }
}

// substitute for Java Object for literals
alias Value = SumType!(double, const(char)[], bool, typeof(null), LoxCall);

enum TokenType {
  // Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  EOF
}

class Token {
    const {
        TokenType type;
        char[] lexeme;
        Value literal;
        int line;
    }

    this(TokenType type, const(char)[] lexeme, Value literal, int line) {
        this.type = type;
        this.lexeme = lexeme;
        this.literal = literal;
        this.line = line;
    }

    override public string toString() const {
        import std.conv;
        return text(type, " ", lexeme, " ", literal);
    }
}
