module lox.compiler;

import lox.scanner;
import lox.io;
import lox.chunk;
import lox.value;

debug = PRINT_CODE;

struct Parser {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
}

enum Precedence {
    NONE,
    ASSIGNMENT,  // =
    OR,          // or
    AND,         // and
    EQUALITY,    // == !=
    COMPARISON,  // < > <= >=
    TERM,        // + -
    FACTOR,      // * /
    UNARY,       // ! -
    CALL,        // . ()
    PRIMARY
}

alias void function(bool canAssign) ParseFn;

struct ParseRule {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
}

Parser parser;
Chunk* compilingChunk;

private Chunk* currentChunk() {
    return compilingChunk;
}

bool compile(const(char)[] source, Chunk* chunk)
{
    initScanner(source);
    compilingChunk = chunk;

    parser.hadError = false;
    parser.panicMode = false;
    advance();
    while (!match(TokenType.EOF)) {
        declaration();
    }
    endCompiler();
    return !parser.hadError;
}

private void advance() {
    parser.previous = parser.current;

    while(true) {
        parser.current = scanToken();
        if(parser.current.type != TokenType.ERROR) break;
        errorAtCurrent(parser.current.lexeme);
    }
}

private void consume(TokenType type, const(char)[] message) {
    if(parser.current.type == type) {
        advance();
        return;
    }

    errorAtCurrent(message);
}

private void emitByte(ubyte b) {
    currentChunk().write(b, parser.previous.line);
}

private void emitBytes(ubyte b1, ubyte b2) {
    emitByte(b1);
    emitByte(b2);
}

private void emitReturn() {
    emitByte(OpCode.RETURN);
}

private ubyte makeConstant(Value value) {
    auto constant = currentChunk().addConstant(value);
    if(constant > ubyte.max) {
        error("Too many constants in one chunk.");
        return 0;
    }
    return cast(ubyte)constant;
}

private void emitConstant(Value value) {
    emitBytes(OpCode.CONSTANT, makeConstant(value));
}

private void endCompiler() {
    emitReturn();
    debug(PRINT_CODE) {
        import lox.dbg;
        if(!parser.hadError)
            disassembleChunk(*currentChunk(), "code");
    }
}

private void binary(bool) {
    TokenType operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(cast(Precedence)(rule.precedence + 1));

    /*with(TokenType)*/ with(OpCode) switch(operatorType) {
        case TokenType.PLUS:          emitByte(ADD); break;
        case TokenType.MINUS:         emitByte(SUBTRACT); break;
        case TokenType.STAR:          emitByte(MULTIPLY); break;
        case TokenType.SLASH:         emitByte(DIVIDE); break;
        case TokenType.EQUAL_EQUAL:   emitByte(EQUAL); break;
        case TokenType.BANG_EQUAL:    emitBytes(EQUAL, NOT); break;
        case TokenType.GREATER:       emitByte(GREATER); break;
        case TokenType.GREATER_EQUAL: emitBytes(LESS, NOT); break;
        case TokenType.LESS:          emitByte(LESS); break;
        case TokenType.LESS_EQUAL:    emitBytes(GREATER, NOT); break;
        default: assert(0); // Unreachable.
    }
}

private void literal(bool) {
    with(TokenType) /*with(Opcode)*/ switch(parser.previous.type) {
        case FALSE: emitByte(OpCode.FALSE); break;
        case NIL: emitByte(OpCode.NIL); break;
        case TRUE: emitByte(OpCode.TRUE); break;
        default: assert(0); // Unreachable.
    }
}

private void expression() {
    parsePrecedence(Precedence.ASSIGNMENT);
}

private void printStatement() {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    emitByte(OpCode.PRINT);
}

private void expressionStatement() {
    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    emitByte(OpCode.POP);
}

private void declaration() {
    if(match(TokenType.VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

private void varDeclaration() {
    ubyte global = parseVariable("Expect variable name.");

    if (match(TokenType.EQUAL)) {
        expression();
    } else {
        emitByte(OpCode.NIL);
    }

    consume(TokenType.SEMICOLON,
            "Expect ';' after vraiable declaration.");

    defineVariable(global);
}

private ubyte parseVariable(string errorMessage) {
    consume(TokenType.IDENTIFIER, errorMessage);
    return identifierConstant(&parser.previous);
}

private ubyte identifierConstant(Token* name) {
    return makeConstant(Value(internString(name.lexeme.idup)));
}

private void defineVariable(ubyte global) {
    emitBytes(OpCode.DEFINE_GLOBAL, global);
}

private void synchronize() {
    parser.panicMode = false;
    with(TokenType) while(parser.current.type != EOF) {
        if(parser.previous.type == SEMICOLON) return;
        switch(parser.current.type) {
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

private void statement() {
    if (match(TokenType.PRINT)) {
        printStatement();
    } else {
        expressionStatement();
    }
}

private bool match(TokenType type) {
    if (!check(type)) return false;
    advance();
    return true;
}

private bool check(TokenType type) => parser.current.type == type;

private void grouping(bool) {
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

private void unary(bool) {
    TokenType operatorType = parser.previous.type;

    // Compile the operand.
    parsePrecedence(Precedence.UNARY);

    with(TokenType) with(OpCode) switch(operatorType) {
        case MINUS: emitByte(NEGATE); break;
        case BANG: emitByte(NOT); break;
        default: return; // Unreachable.
    }
}

ParseRule[] rules = [
  TokenType.LEFT_PAREN:    ParseRule(&grouping, null,    Precedence.NONE),
  TokenType.RIGHT_PAREN:   ParseRule(null,      null,    Precedence.NONE),
  TokenType.LEFT_BRACE:    ParseRule(null,      null,    Precedence.NONE), 
  TokenType.RIGHT_BRACE:   ParseRule(null,      null,    Precedence.NONE),
  TokenType.COMMA:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.DOT:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.MINUS:         ParseRule(&unary,    &binary, Precedence.TERM),
  TokenType.PLUS:          ParseRule(null,      &binary, Precedence.TERM),
  TokenType.SEMICOLON:     ParseRule(null,      null,    Precedence.NONE),
  TokenType.SLASH:         ParseRule(null,      &binary, Precedence.FACTOR),
  TokenType.STAR:          ParseRule(null,      &binary, Precedence.FACTOR),
  TokenType.BANG:          ParseRule(&unary,    null,    Precedence.NONE),
  TokenType.BANG_EQUAL:    ParseRule(&binary,   null,    Precedence.EQUALITY),
  TokenType.EQUAL:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.EQUAL_EQUAL:   ParseRule(null,      &binary, Precedence.EQUALITY),
  TokenType.GREATER:       ParseRule(null,      &binary, Precedence.COMPARISON),
  TokenType.GREATER_EQUAL: ParseRule(null,      &binary, Precedence.COMPARISON),
  TokenType.LESS:          ParseRule(null,      &binary, Precedence.COMPARISON),
  TokenType.LESS_EQUAL:    ParseRule(null,      &binary, Precedence.COMPARISON),
  TokenType.IDENTIFIER:    ParseRule(&variable, null,    Precedence.NONE),
  TokenType.STRING:        ParseRule(&str,      null,    Precedence.NONE),
  TokenType.NUMBER:        ParseRule(&number,   null,    Precedence.NONE),
  TokenType.AND:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.CLASS:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.ELSE:          ParseRule(null,      null,    Precedence.NONE),
  TokenType.FALSE:         ParseRule(&literal,  null,    Precedence.NONE),
  TokenType.FOR:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.FUN:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.IF:            ParseRule(null,      null,    Precedence.NONE),
  TokenType.NIL:           ParseRule(&literal,  null,    Precedence.NONE),
  TokenType.OR:            ParseRule(null,      null,    Precedence.NONE),
  TokenType.PRINT:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.RETURN:        ParseRule(null,      null,    Precedence.NONE),
  TokenType.SUPER:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.THIS:          ParseRule(null,      null,    Precedence.NONE),
  TokenType.TRUE:          ParseRule(&literal,  null,    Precedence.NONE),
  TokenType.VAR:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.WHILE:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.ERROR:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.EOF:           ParseRule(null,      null,    Precedence.NONE)
];

private void parsePrecedence(Precedence precedence) {
    advance();
    auto prefixRule = getRule(parser.previous.type).prefix;
    if(prefixRule is null) {
        error("Expect expression.");
        return;
    }

    auto canAssign = precedence <= Precedence.ASSIGNMENT;
    prefixRule(canAssign);

    while(precedence <= getRule(parser.current.type).precedence) {
        advance();
        auto infixRule = getRule(parser.previous.type).infix;
        assert(infixRule);
        infixRule(canAssign);
    }

    if(canAssign && match(TokenType.EQUAL)) {
        error("Invalid assignment target.");
    }
}

private ParseRule* getRule(TokenType type) => &rules[type];

private void number(bool) {
    import std.conv;
    Value value = Value(parser.previous.lexeme.to!double);
    emitConstant(value);
}

private void str(bool) {
    // idup all strings, not sure when the source will go away.
    Value value = Value(internString(parser.previous.lexeme[1 .. $-1].idup));
    emitConstant(value);
}

private void variable(bool canAssign) {
    namedVariable(parser.previous, canAssign);
}

private void namedVariable(Token name, bool canAssign) {
    ubyte arg = identifierConstant(&name);
    if (canAssign && match(TokenType.EQUAL)) {
        expression();
        emitBytes(OpCode.SET_GLOBAL, arg);
    } else {
        emitBytes(OpCode.GET_GLOBAL, arg);
    }
}

private void errorAtCurrent(const(char)[] message) {
    errorAt(&parser.current, message);
}

private void error(const(char)[] message) {
    errorAt(&parser.previous, message);
}

private void errorAt(Token* token, const(char)[] message) {
    if(parser.panicMode) return;
    parser.panicMode = true;
    auto o = errStream;
    o.write(i`[line $(token.line)] Error`, false);

    if(token.type == TokenType.EOF) {
        o.write(" at end", false);
    } else if(token.type == TokenType.ERROR) {
        // Nothing.
    } else {
        o.write(i` at '$(token.lexeme)'`, false);
    }

    o.writeln(i`: $(message)`);
    parser.hadError = true;
}
