module lox.parser;

import lox.token;
import lox.ast;

class Parser {
    private Token[] tokens;
    private int current = 0;

    this(Token[] tokens) {
        this.tokens = tokens;
    }

    Expr parse() {
        try {
            return expression();
        } catch (ParseError error) {
            return null;
        }
    }


    private Expr expression() {
        return equality();
    }

    private Expr equality() {
        Expr expr = comparison();

        while(match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Binary(expr, operator, right);
        }
        return expr;
    }

    private Expr comparison() {
        Expr expr = term();

        while(match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
            Token operator = previous();
            Expr right = term();
            expr = new Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr term() {
        Expr expr = factor();

        while(match(TokenType.PLUS, TokenType.MINUS)) {
            Token operator = previous();
            Expr right = factor();
            expr = new Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr factor() {
        Expr expr = unary();

        while(match(TokenType.SLASH, TokenType.STAR)) {
            Token operator = previous();
            Expr right = unary();
            expr = new Binary(expr, operator, right);
        }

        return expr;
    }

    private Expr unary() {
        if(match(TokenType.BANG, TokenType.MINUS))
        {
            Token operator = previous();
            Expr right = unary();
            return new Unary(operator, right);
        }
        return primary();
    }

    private Expr primary() {
        if(match(TokenType.TRUE)) return new Literal(Value(true));
        if(match(TokenType.FALSE)) return new Literal(Value(false));
        if(match(TokenType.NIL)) return new Literal(Value(null));

        if(match(TokenType.NUMBER, TokenType.STRING))
            return new Literal(previous().literal);

        if(match(TokenType.LEFT_PAREN))
        {
            Expr container = expression();
            consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
            return new Grouping(container);
        }

        // error
        throw error(peek(), "Expect expression.");
    }

    private bool match(TokenType[] types...) {
        foreach(type; types) {
            if(check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    private bool check(TokenType type) {
        if(isAtEnd()) return false;
        return peek().type == type;
    }

    private Token advance() {
        if (!isAtEnd()) ++current;
        return previous();
    }

    private Token consume(TokenType type, string message) {
        if(check(type)) return advance();
        throw error(peek(), message);
    }

    private ParseError error(Token token, string message) {
        static import lox.lox;
        lox.lox.error(token, message);
        return new ParseError();
    }

    private void synchronize() {
        advance();
        while(!isAtEnd()) {
            if(previous().type == TokenType.SEMICOLON) return;
            with(TokenType) switch(peek().type) {
                case CLASS:
                case FUN:
                case VAR:
                case FOR:
                case IF:
                case WHILE:
                case PRINT:
                case RETURN:
                    return;
                default:
                    break;
            }

            advance();
        }
    }

    private bool isAtEnd() => peek().type == TokenType.EOF;
    private Token peek() => tokens[current];
    private Token previous() => tokens[current-1];
}

private class ParseError : Exception {
    this(string file = __FILE__, size_t line = __LINE__) {
        super("Parse error", file, line);
    }
}
