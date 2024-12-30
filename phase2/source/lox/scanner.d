module lox.scanner;

enum TokenType {
  // Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN,
  LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS,
  SEMICOLON, SLASH, STAR,
  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,
  // Literals.
  IDENTIFIER, STRING, NUMBER,
  // Keywords.
  AND, CLASS, ELSE, FALSE,
  FOR, FUN, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS,
  TRUE, VAR, WHILE,

  ERROR, EOF
}

struct Token
{
    TokenType type;
    const(char)[] lexeme;
    int line;
}

struct Scanner
{
    const(char)* start;
    const(char)* current;
    const(char)* end;
    int line;

    void initialize(const(char)[] source) {
        start = source.ptr;
        current = start;
        end = start + source.length;
        line = 1;
    }

    Token scanToken() {
        skipWhitespace();
        start = current;
        if (isAtEnd()) return makeToken(TokenType.EOF);

        char c = advance();

        if(isAlpha(c)) return makeIdentifier();
        if(isDigit(c)) return makeNumber();

        with(TokenType) switch(c) {
            case '(': return makeToken(LEFT_PAREN);
            case ')': return makeToken(RIGHT_PAREN);
            case '{': return makeToken(LEFT_BRACE);
            case '}': return makeToken(RIGHT_BRACE);
            case ';': return makeToken(SEMICOLON);
            case ',': return makeToken(COMMA);
            case '.': return makeToken(DOT);
            case '-': return makeToken(MINUS);
            case '+': return makeToken(PLUS);
            case '/': return makeToken(SLASH);
            case '*': return makeToken(STAR);
            case '!':
                      return makeToken(
                              match('=') ? BANG_EQUAL : BANG);
            case '=':
                      return makeToken(
                              match('=') ? EQUAL_EQUAL : EQUAL);
            case '<':
                      return makeToken(
                              match('=') ? LESS_EQUAL : LESS);
            case '>':
                      return makeToken(
                              match('=') ? GREATER_EQUAL : GREATER);
            case '"': return makeString();
            default: break;
        }

        return errorToken("Unexpected character.");
    }

    char advance() {
        assert(current !is end);
        return *current++;
    }

    bool match(char next) {
        if (isAtEnd()) return false;
        if (*current != next) return false;
        ++current;
        return true;
    }

    bool isDigit(char c) => c >= '0' && c <= '9';

    bool isAlpha(char c) =>
        (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
         c == '_';

    bool isAtEnd() => current is end;

    void skipWhitespace() {
        while(true) {
            char c = *current;
            switch(c) {
                case ' ':
                case '\r':
                case '\t':
                    advance();
                    break;
                case '\n':
                    ++line;
                    advance();
                    break;
                case '/':
                    if(peekNext() == '/') {
                        // A comment goes until the end of the line.
                        while (peek() != '\n' && !isAtEnd()) advance();
                    } else {
                        return;
                    }
                    break;
                default:
                    return;
            }
        }
    }

    char peek() {
        if(isAtEnd()) return '\0';
        return *current;
    }

    char peekNext() {
        if(current + 1 >= end)
            return '\0';
        return current[1];
    }

    Token makeToken(TokenType type) {
        auto token = Token(
                type: type,
                lexeme: start[0 .. current - start],
                line: line
                );
        return token;
    }

    Token errorToken(string message) {
        auto token = Token(
                type: TokenType.ERROR,
                lexeme: message,
                line: line
                );
        return token;
    }

    Token makeString() {
        auto n = peek();
        while(n != '\0' && n != '"') {
            if(n == '\n') ++line;
            advance();
            n = peek();
        }

        if(n == '\0') return errorToken("Unterminated string.");

        // the closing quote.
        advance();
        return makeToken(TokenType.STRING);
    }

    Token makeNumber() {
        while(isDigit(peek())) advance();
        if (peek() == '.' && isDigit(peekNext())) {
            // Consume the '.'
            advance();

            while(isDigit(peek())) advance();
        }

        return makeToken(TokenType.NUMBER);
    }

    Token makeIdentifier() {
        while(isAlpha(peek()) || isDigit(peek())) advance();
        return makeToken(identifierType());
    }

    TokenType identifierType() {
        with(TokenType) switch(start[0]) {
            case 'a': return checkKeyword(1, "nd", AND);
            case 'c': return checkKeyword(1, "lass", CLASS);
            case 'e': return checkKeyword(1, "lse", ELSE);
            case 'f':
                if( current - start > 1) {
                    switch (start[1]) {
                        case 'a': return checkKeyword(2, "lse", FALSE);
                        case 'o': return checkKeyword(2, "r", FOR);
                        case 'u': return checkKeyword(2, "n", FUN);
                        default: break;
                    }
                }
                break;
            case 'i': return checkKeyword(1, "f", IF);
            case 'n': return checkKeyword(1, "il", NIL);
            case 'o': return checkKeyword(1, "r", OR);
            case 'p': return checkKeyword(1, "rint", PRINT);
            case 'r': return checkKeyword(1, "eturn", RETURN);
            case 's': return checkKeyword(1, "uper", SUPER);

            case 't':
                if (scanner.current - scanner.start > 1) {
                    switch (scanner.start[1]) {
                        case 'h': return checkKeyword(2, "is", THIS);
                        case 'r': return checkKeyword(2, "ue", TRUE);
                        default: break;
                    }
                }
                break;
            case 'v': return checkKeyword(1, "ar", VAR);
            case 'w': return checkKeyword(1, "hile", WHILE);
            default: break;
        }

        return TokenType.IDENTIFIER;
    }

    TokenType checkKeyword(int sidx, string match, TokenType type)
    {
        if(current - start - sidx == match.length &&
                start[sidx .. sidx + match.length] == match)
        {
            return type;
        }
        return TokenType.IDENTIFIER;
    }
}

Scanner scanner;

void initScanner(const(char)[] source)
{
    scanner.initialize(source);
}

Token scanToken()
{
    return scanner.scanToken();
}
