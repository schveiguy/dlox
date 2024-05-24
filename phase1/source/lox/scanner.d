module lox.scanner;

import lox.token;
import lox.lox;

static immutable keywords = [
    "and"    : TokenType.AND,
    "class"  : TokenType.CLASS,
    "else"   : TokenType.ELSE,
    "false"  : TokenType.FALSE,
    "for"    : TokenType.FOR,
    "fun"    : TokenType.FUN,
    "if"     : TokenType.IF,
    "nil"    : TokenType.NIL,
    "or"     : TokenType.OR,
    "print"  : TokenType.PRINT,
    "return" : TokenType.RETURN,
    "super"  : TokenType.SUPER,
    "this"   : TokenType.THIS,
    "true"   : TokenType.TRUE,
    "var"    : TokenType.VAR,
    "while"  : TokenType.WHILE
];

class Scanner {
    private {
        const(char)[] source;
        Token[] tokens;
        int start = 0;
        int current = 0;
        int line = 1;
    }

    this(const(char)[] source) {
        this.source = source;
    }

    Token[] scanTokens() {
        while(!isAtEnd())
        {
            // We are at the beginning of the next lexeme.
            start = current;
            scanToken();
        }

        tokens ~= new Token(TokenType.EOF, "", Literal(null), line);
        return tokens;
    }

    private void scanToken() {
        char c = advance();
        with(TokenType) switch(c) {
        case '(': addToken(LEFT_PAREN); break;
        case ')': addToken(RIGHT_PAREN); break;
        case '{': addToken(LEFT_BRACE); break;
        case '}': addToken(RIGHT_BRACE); break;
        case ',': addToken(COMMA); break;
        case '.': addToken(DOT); break;
        case '-': addToken(MINUS); break;
        case '+': addToken(PLUS); break;
        case ';': addToken(SEMICOLON); break;
        case '*': addToken(STAR); break;
        case '!':
            addToken(match('=') ? BANG_EQUAL : BANG);
            break;
        case '=':
            addToken(match('=') ? EQUAL_EQUAL : EQUAL);
            break;
        case '<':
            addToken(match('=') ? LESS_EQUAL : LESS);
            break;
        case '>':
            addToken(match('=') ? GREATER_EQUAL : GREATER);
            break;
        case '/':
            if (match('/')) {
                // a comment goes until the end of the line.
                while (peek() != '\n' && !isAtEnd()) advance();
            } else {
                addToken(SLASH);
            }
            break;
        case ' ':
        case '\r':
        case '\t':
            // Ignore whitespace.
            break;

        case '\n':
            ++line;
            break;

        case '"': string(); break;

        default:
            if(isDigit(c)) {
                number();
            } else if (isAlpha(c)) {
                identifier();
            } else {
                .error(line, "Unexpected chararacter.");
            }
            break;
        }
    }

    // helpers
    private bool isAtEnd() => current >= source.length;
    private char advance() => source[current++];
    private char peek() => isAtEnd() ? '\0' : source[current];
    private char peekNext() => current + 1 >= source.length ? '\0' : source[current + 1];
    private bool isDigit(char c) => c >= '0' && c <= '9';
    private bool isAlpha(char c) => (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';

    private void addToken(TokenType type, Literal literal = Literal(null)) {
        tokens ~= new Token(type, source[start .. current], literal, line);
    }

    private bool match(char expected) {
        if (isAtEnd()) return false;
        if (source[current] != expected) return false;
        ++current;
        return true;
    }

    private void string() {
        while(peek() != '"' && !isAtEnd()) {
            if(peek() == '\n') ++line;
            advance();
        }

        if(isAtEnd()) {
            .error(line, "Unterminated string.");
            return;
        }
        // the closing ".
        advance();

        // trim the surrounding quotes
        auto lit = Literal(source[start + 1 .. current - 1]);
        addToken(TokenType.STRING, lit);
    }

    private void number() {
        while(isDigit(peek())) advance();
        if(peek() == '.' && isDigit(peekNext())) {
            advance();

            while(isDigit(peek())) advance();
        }

        import std.conv : to;
        auto lit = Literal(source[start .. current].to!double);
        addToken(TokenType.NUMBER, lit);
    }

    private void identifier() {
        while(isAlpha(peek()) || isDigit(peek())) advance();

        auto id = source[start .. current];

        if (auto kw = id in keywords) {
            addToken(*kw);
        } else {
            addToken(TokenType.IDENTIFIER);
        }
    }
}
