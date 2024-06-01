module lox.parser;

import lox.token;
import lox.value;
import lox.ast;

final class Parser {
    private Token[] tokens;
    private int current = 0;

    this(Token[] tokens) {
        this.tokens = tokens;
    }

    // entry point
    Stmt[] parse() {
        Stmt[] statements;
        while(!isAtEnd())
            statements ~= declaration();

        return statements;
    }

private:
    // grammar productions
    Stmt declaration()
    {
        try {
            if(match(TokenType.CLASS))
                return classDecl();
            if(match(TokenType.VAR))
                return varDecl();
            if(match(TokenType.FUN))
                return funDecl("function");
            return statement();
        } catch(ParseError error) {
            synchronize();
            return null;
        }
    }

    Stmt varDecl()
    {
        auto name = consume(TokenType.IDENTIFIER, "Expect variable name.");
        Expr initializer;
        if(match(TokenType.EQUAL))
        {
            initializer = expression();
        }
        consume(TokenType.SEMICOLON, "Expect ';' after declaration.");
        return new Var(name, initializer);
    }

    Function funDecl(string kind)
    {
        auto name = consume(TokenType.IDENTIFIER, "Expect " ~ kind ~ " name.");
        consume(TokenType.LEFT_PAREN, "Expect '(' after " ~ kind ~ " name.");
        Token[] parameters;
        if(!check(TokenType.RIGHT_PAREN))
            do
            {
                if(parameters.length >= 255)
                    error(peek(), "Can't have more than 255 parameters.");
                parameters ~= consume(TokenType.IDENTIFIER, "Expect parameter name.");
            } while(match(TokenType.COMMA));
        consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");
        consume(TokenType.LEFT_BRACE, "Expect '{' before " ~ kind ~ " body.");
        return new Function(name, parameters, block());
    }

    Stmt classDecl() {
        auto className = consume(TokenType.IDENTIFIER, "Expect name for class.");

        Variable superclass = null;
        if(match(TokenType.LESS))
        {
            consume(TokenType.IDENTIFIER, "Expect superclass name.");
            superclass = new Variable(previous());
        }
        consume(TokenType.LEFT_BRACE, "Expect '{' after class name.");
        Function[] methods;
        while(!check(TokenType.RIGHT_BRACE) && !isAtEnd())
            methods ~= funDecl("method");

        consume(TokenType.RIGHT_BRACE, "Expect '}' after class definition.");

        return new Class(className, superclass, methods);
    }

    Stmt statement() {
        if(match(TokenType.PRINT)) return printStatement();
        if(match(TokenType.LEFT_BRACE)) return blockStatement();
        if(match(TokenType.IF)) return ifStatement();
        if(match(TokenType.FOR)) return forStatement();
        if(match(TokenType.WHILE)) return whileStatement();
        if(match(TokenType.RETURN)) return returnStatement();
        return expressionStatement();
    }

    Stmt expressionStatement() {
        auto expr = expression();
        consume(TokenType.SEMICOLON, "Expect ';' after expression.");
        return new Expression(expr);
    }

    Stmt printStatement() {
        auto expr = expression();
        consume(TokenType.SEMICOLON, "Expect ';' after value.");
        return new Print(expr);
    }

    Stmt blockStatement() {
        return new Block(block());
    }

    Stmt ifStatement() {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
        auto cond = expression();
        consume(TokenType.RIGHT_PAREN, "Expect ')' after if condition.");
        auto thenBranch = statement();
        Stmt elseBranch = null;
        if(match(TokenType.ELSE))
        {
            elseBranch = statement();
        }
        return new If(cond, thenBranch, elseBranch);
    }

    Stmt whileStatement() {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
        auto cond = expression();
        consume(TokenType.RIGHT_PAREN, "Expect ')' after while condition.");
        auto body = statement();
        return new While(cond, body);
    }

    Stmt forStatement() {
        consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
        Stmt declaration = null;
        Expr condition = null;
        Expr increment = null;
        if(!match(TokenType.SEMICOLON))
        {
            if(match(TokenType.VAR))
                declaration = varDecl();
            else
                declaration = expressionStatement();
        }
        if(!match(TokenType.SEMICOLON))
        {
            condition = expression();
            consume(TokenType.SEMICOLON, "Expect ';' after for condition.");
        }
        if(!match(TokenType.RIGHT_PAREN))
        {
            increment = expression();
            consume(TokenType.RIGHT_PAREN, "Expect ')' after for increment.");
        }
        auto body = statement();
        return new For(declaration, condition, increment, body);
    }

    Stmt returnStatement() {
        Token keyword = previous();
        Expr retval = null;
        if(!check(TokenType.SEMICOLON))
            retval = expression();

        consume(TokenType.SEMICOLON, "Expect ';' after return statement.");
        return new Return(keyword, retval);
    }

    Stmt[] block() {
        Stmt[] statements;
        while(!isAtEnd())
        {
            if(match(TokenType.RIGHT_BRACE))
                return statements;
            statements ~= declaration();
        }
        throw error(peek(), "Expected right brace");
    }

    Expr expression() {
        return assignment();
    }

    Expr assignment() {
        // parse the left expression
        auto expr = logicOr();
        if(match(TokenType.EQUAL))
        {
            Token equals = previous();
            Expr value = assignment();
            if(auto var = cast(Variable)expr)
            {
                return new Assign(var.name, value);
            }
            else if(auto get = cast(Get)expr)
            {
                return new Set(get.obj, get.name, value);
            }

            error(equals, "Invalid assignment target.");
        }

        return expr;
    }

    Expr logicOr() {
        auto expr = logicAnd();
        while(match(TokenType.OR))
        {
            Token operator = previous();
            Expr right = logicAnd();
            expr = new Logical(expr, operator, right);
        }

        return expr;
    }

    Expr logicAnd() {
        auto expr = equality();
        while(match(TokenType.AND))
        {
            Token operator = previous();
            Expr right = equality();
            expr = new Logical(expr, operator, right);
        }

        return expr;
    }

    Expr equality() {
        Expr expr = comparison();

        while(match(TokenType.BANG_EQUAL, TokenType.EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Binary(expr, operator, right);
        }
        return expr;
    }

    Expr comparison() {
        Expr expr = term();

        while(match(TokenType.GREATER, TokenType.GREATER_EQUAL, TokenType.LESS, TokenType.LESS_EQUAL)) {
            Token operator = previous();
            Expr right = term();
            expr = new Binary(expr, operator, right);
        }

        return expr;
    }

    Expr term() {
        Expr expr = factor();

        while(match(TokenType.PLUS, TokenType.MINUS)) {
            Token operator = previous();
            Expr right = factor();
            expr = new Binary(expr, operator, right);
        }

        return expr;
    }

    Expr factor() {
        Expr expr = unary();

        while(match(TokenType.SLASH, TokenType.STAR)) {
            Token operator = previous();
            Expr right = unary();
            expr = new Binary(expr, operator, right);
        }

        return expr;
    }

    Expr unary() {
        if(match(TokenType.BANG, TokenType.MINUS))
        {
            Token operator = previous();
            Expr right = unary();
            return new Unary(operator, right);
        }
        return call();
    }

    Expr call() {
        auto callee = primary();

        void finishCall()
        {
            Expr[] arguments;
            if(!check(TokenType.RIGHT_PAREN))
                do
                {
                    if(arguments.length >= 255)
                        error(peek(), "Can't have more than 255 arguments.");
                    arguments ~= expression();
                } while(match(TokenType.COMMA));
            auto paren = consume(TokenType.RIGHT_PAREN, "Expect ')' after function arugments.");
            callee = new Call(callee, paren, arguments);
        }

        void finishGet() {
            auto name = consume(TokenType.IDENTIFIER, "Expect identifier after object get syntax.");
            callee = new Get(callee, name);
        }

        while(true)
        {
            if(match(TokenType.DOT))
                finishGet();
            else if(match(TokenType.LEFT_PAREN))
                finishCall();
            else
                break;
        }

        return callee;
    }

    Expr primary() {
        if(match(TokenType.TRUE)) return new Literal(Value(true));
        if(match(TokenType.FALSE)) return new Literal(Value(false));
        if(match(TokenType.NIL)) return new Literal(Value(null));
        if(match(TokenType.IDENTIFIER)) return new Variable(previous());
        if(match(TokenType.THIS)) return new This(previous());
        if(match(TokenType.SUPER)) {
            auto keyword = previous();
            consume(TokenType.DOT, "Expect '.' after 'super'.");
            auto method = consume(TokenType.IDENTIFIER, "Expect superclass method name.");
            return new Super(keyword, method);
        }

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

    // helpers
    bool match(TokenType[] types...) {
        foreach(type; types) {
            if(check(type)) {
                advance();
                return true;
            }
        }

        return false;
    }

    bool check(TokenType type) {
        if(isAtEnd()) return false;
        return peek().type == type;
    }

    Token advance() {
        if (!isAtEnd()) ++current;
        return previous();
    }

    Token consume(TokenType type, string message) {
        if(check(type)) return advance();
        throw error(peek(), message);
    }

    ParseError error(Token token, string message) {
        static import lox.lox;
        lox.lox.error(token, message);
        return new ParseError();
    }

    void synchronize() {
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

    bool isAtEnd() => peek().type == TokenType.EOF;
    Token peek() => tokens[current];
    Token previous() => tokens[current-1];
}

private final class ParseError : Exception {
    this(string file = __FILE__, size_t line = __LINE__) {
        super("Parse error", file, line);
    }
}
