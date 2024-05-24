module lox.token;

import lox.tokentype;

import std.sumtype;

// substitute for Java Object for literals
alias Literal = SumType!(double, const(char)[], bool, typeof(null));


class Token {
    const {
        TokenType type;
        char[] lexeme;
        Literal literal;
        int line;
    }

    this(TokenType type, const(char)[] lexeme, Literal literal, int line) {
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
