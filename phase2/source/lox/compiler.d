module lox.compiler;

import lox.scanner;
import lox.io;
import lox.chunk;
import lox.value;
import lox.obj;

struct Parser {
    Token current;
    Token previous;
    bool hadError;
    bool panicMode;
}

enum FunctionType {
    FUNCTION,
    INITIALIZER,
    METHOD,
    SCRIPT
}

struct Upvalue {
    ubyte index;
    bool isLocal;
}

struct Compiler {
    Compiler *enclosing;
    ObjFunction *fun;
    FunctionType type = FunctionType.SCRIPT;

    Local[ubyte.max + 1] locals;
    int localCount;
    Upvalue[ubyte.max + 1] upvalues;
    int scopeDepth;
}

struct ClassCompiler {
    ClassCompiler* enclosing;
    bool hasSuperclass;
}

struct Local {
    Token name;
    int depth;
    bool isCaptured;
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
Compiler* current;
ClassCompiler* currentClass;

private void initCompiler(Compiler * compiler, FunctionType type) {
    compiler.enclosing = current;
    compiler.fun = new ObjFunction;
    compiler.type = type;
    compiler.localCount = 0;
    compiler.scopeDepth = 0;
    current = compiler;

    if(type != FunctionType.SCRIPT) {
        current.fun.name = internString(parser.previous.lexeme.idup);
    }

    Local* local = &current.locals[current.localCount++];
    local.depth = 0;
    if (type != FunctionType.FUNCTION)
        local.name = thisToken;
    else
        local.name = Token.init;
}

private Chunk* currentChunk() {
    return &current.fun.chunk;
}

ObjFunction* compile(const(char)[] source)
{
    initScanner(source);
    Compiler compiler;
    initCompiler(&compiler, FunctionType.SCRIPT);

    parser.hadError = false;
    parser.panicMode = false;
    advance();
    while (!match(TokenType.EOF)) {
        declaration();
    }
    auto fun = endCompiler();
    return parser.hadError ? null : fun;
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
    if (current.type == FunctionType.INITIALIZER) {
        emitBytes(OpCode.GET_LOCAL, 0);
    }
    else {
        emitByte(OpCode.NIL);
    }
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

private ObjFunction* endCompiler() {
    emitReturn();

    auto fun = current.fun;
    debug(PRINT_CODE) {
        import lox.dbg;
        if(!parser.hadError)
        {
            auto n = fun.name ? fun.name.value : "";
            disassembleChunk(*currentChunk(), n.length > 0 ? n : "<script>");
        }
    }
    current = current.enclosing;
    return fun;
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

private void call(bool) {
    ubyte argCount = argumentList();
    emitBytes(OpCode.CALL, argCount);
}

private void dot(bool canAssign) {
    consume(TokenType.IDENTIFIER, "Expect property name after '.'.");
    ubyte name = identifierConstant(&parser.previous);

    if (canAssign && match(TokenType.EQUAL)) {
        expression();
        emitBytes(OpCode.SET_PROPERTY, name);
    } else if (match(TokenType.LEFT_PAREN)) {
        ubyte argCount = argumentList();
        emitBytes(OpCode.INVOKE, name);
        emitByte(argCount);
    } else {
        emitBytes(OpCode.GET_PROPERTY, name);
    }
}

private ubyte argumentList() {
    ubyte argCount = 0;
    if (!check(TokenType.RIGHT_PAREN)) {
        do {
            expression();
            if(argCount == ubyte.max) {
                error("Can't have more than 255 arguments.");
            }
            ++argCount;
        } while(match(TokenType.COMMA));
    }

    consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return argCount;
}

private void literal(bool) {
    with(TokenType) /*with(OpCode)*/ switch(parser.previous.type) {
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
    if (match(TokenType.CLASS)) {
        classDeclaration();
    } else if(match(TokenType.FUN)) {
        funDeclaration();
    } else if(match(TokenType.VAR)) {
        varDeclaration();
    } else {
        statement();
    }

    if (parser.panicMode) synchronize();
}

private void funDeclaration() {
    ubyte global = parseVariable("Expect function name.");
    markInitialized();
    func(FunctionType.FUNCTION);
    defineVariable(global);
}

private void func(FunctionType type) {
    Compiler compiler;
    initCompiler(&compiler, type);
    beginScope();
    consume(TokenType.LEFT_PAREN, "Expect '(' after function name.");
    if (!check(TokenType.RIGHT_PAREN)) {
        do {
            current.fun.arity++;
            if(current.fun.arity > 255) {
                errorAtCurrent("Can't have more than 255 parameters.");
            }
            ubyte constant = parseVariable("Expect parameter name.");
            defineVariable(constant);
        } while(match(TokenType.COMMA));
    }
    consume(TokenType.RIGHT_PAREN, "Expect '(' after function name.");
    consume(TokenType.LEFT_BRACE, "Expect '{' before function body.");
    block();
    auto fun = endCompiler();
    emitBytes(OpCode.CLOSURE, makeConstant(Value(fun)));

    foreach(uv; compiler.upvalues[0 .. fun.upvalueCount]) {
        emitByte(uv.isLocal ? 1 : 0);
        emitByte(uv.index);
    }
}

private void classDeclaration() {
    consume(TokenType.IDENTIFIER, "Expect class name.");
    auto className = parser.previous;
    ubyte nameConstant = identifierConstant(&parser.previous);
    declareVariable();

    emitBytes(OpCode.CLASS, nameConstant);
    defineVariable(nameConstant);

    ClassCompiler classCompiler;
    classCompiler.enclosing = currentClass;
    currentClass = &classCompiler;

    if (match(TokenType.LESS)) {
        consume(TokenType.IDENTIFIER, "Expect superclass name.");
        variable(false);

        if(className.lexeme == parser.previous.lexeme) {
            error("A class can't inherit from itself.");
        }

        beginScope();
        addLocal(superToken);
        defineVariable(0);

        namedVariable(className, false);
        emitByte(OpCode.INHERIT);
        classCompiler.hasSuperclass = true;
    }

    namedVariable(className, false);
    consume(TokenType.LEFT_BRACE, "Expect '{' before class body.");
    while (!check(TokenType.RIGHT_BRACE) && !check(TokenType.EOF)) {
        method();
    }
    consume(TokenType.RIGHT_BRACE, "Expect '}' after class body.");
    emitByte(OpCode.POP);

    if (classCompiler.hasSuperclass) {
        endScope();
    }

    currentClass = currentClass.enclosing;
}

private void method() {
    consume(TokenType.IDENTIFIER, "Expect method name.");
    ubyte constant = identifierConstant(&parser.previous);

    FunctionType type = FunctionType.METHOD;
    if (parser.previous.lexeme == "init") {
        type = FunctionType.INITIALIZER;
    }
    func(type);
    emitBytes(OpCode.METHOD, constant);
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

    declareVariable();
    if(current.scopeDepth > 0) return 0;

    return identifierConstant(&parser.previous);
}

private ubyte identifierConstant(Token* name) {
    return makeConstant(Value(internString(name.lexeme.idup)));
}

private void defineVariable(ubyte global) {
    if (current.scopeDepth > 0) {
        markInitialized();
        return;
    }

    emitBytes(OpCode.DEFINE_GLOBAL, global);
}

private void and_(bool) {
    int endJump = emitJump(OpCode.JUMP_IF_FALSE);
    emitByte(OpCode.POP);

    parsePrecedence(Precedence.AND);

    patchJump(endJump);
}

private void or_(bool) {
    int elseJump = emitJump(OpCode.JUMP_IF_FALSE);
    int endJump = emitJump(OpCode.JUMP);

    patchJump(elseJump);
    emitByte(OpCode.POP);

    parsePrecedence(Precedence.OR);

    patchJump(endJump);
}

private void markInitialized() {
    if (current.scopeDepth == 0) return;
    current.locals[current.localCount - 1].depth = current.scopeDepth;
}

private void declareVariable() {
    if (current.scopeDepth == 0) return;

    auto name = &parser.previous;

    foreach_reverse(i; 0 .. current.localCount) {
        auto local = &current.locals[i];
        if(local.depth != -1 && local.depth < current.scopeDepth) {
            break;
        }

        if(name.lexeme == local.name.lexeme) {
            error("Already a variable with this name in the scope.");
        }
    }
    addLocal(*name);
}

private void addLocal(Token name) {
    if(current.localCount == current.locals.length) {
        error("Too many local variables in function.");
        return;
    }
    Local* local = &current.locals[current.localCount++];
    local.name = name;
    local.depth = -1;
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
    } else if (match(TokenType.RETURN)) {
        returnStatement();
    } else if (match(TokenType.IF)) {
        ifStatement();
    } else if (match(TokenType.WHILE)) {
        whileStatement();
    } else if (match(TokenType.FOR)) {
        forStatement();
    } else if (match(TokenType.LEFT_BRACE)) {
        beginScope();
        block();
        endScope();
    } else {
        expressionStatement();
    }
}

private void returnStatement() {
    if(current.type == FunctionType.SCRIPT) {
        error("Can't return from top-level code.");
    }
    if(match(TokenType.SEMICOLON)) {
        emitReturn();
        return;
    }
    else if (current.type == FunctionType.INITIALIZER) {
        error("Can't return a value from an initializer.");
    }

    expression();
    consume(TokenType.SEMICOLON, "Expect ';' after return value.");
    emitByte(OpCode.RETURN);
}

private void ifStatement() {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");
    int thenJump = emitJump(OpCode.JUMP_IF_FALSE);
    emitByte(OpCode.POP);
    statement();

    int elseJump = emitJump(OpCode.JUMP);

    patchJump(thenJump);

    emitByte(OpCode.POP);

    if (match(TokenType.ELSE)) statement();

    patchJump(elseJump);
}

private void whileStatement() {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    auto loopTarget = currentChunk.count;
    expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");
    int thenJump = emitJump(OpCode.JUMP_IF_FALSE);
    emitByte(OpCode.POP);
    statement();

    emitLoop(loopTarget);

    patchJump(thenJump);

    emitByte(OpCode.POP);
}

private void forStatement() {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
    // initializer
    beginScope();
    if(match(TokenType.VAR)) {
        varDeclaration();
    } else if(match(TokenType.SEMICOLON)) {
        // no initializer.
    } else {
        expressionStatement();
    }

    // condition
    auto loopStart = currentChunk().count;
    int conditionJump = -1;
    bool hasCondition = !match(TokenType.SEMICOLON);
    if(hasCondition) {
        expression();
        consume(TokenType.SEMICOLON, "For condition must be followed by semicolon.");
        conditionJump = emitJump(OpCode.JUMP_IF_FALSE);
        emitByte(OpCode.POP);
    }

    // increment
    if(!match(TokenType.RIGHT_PAREN)) {
        int skipToBody = emitJump(OpCode.JUMP);
        auto incrementStart = currentChunk().count;
        expression();
        emitByte(OpCode.POP);
        consume(TokenType.RIGHT_PAREN, "For condition must be followed by semicolon.");
        emitLoop(loopStart);
        loopStart = incrementStart;
        patchJump(skipToBody);
    }

    statement();

    emitLoop(loopStart);

    if(hasCondition) {
        patchJump(conditionJump);
        emitByte(OpCode.POP);
    }
    endScope();
}

private int emitJump(ubyte instruction) {
    emitByte(instruction);
    emitByte(0xff);
    emitByte(0xff);
    return currentChunk().count - 2;
}

private void emitLoop(int target) {
    emitByte(OpCode.LOOP);
    auto diff = currentChunk.count + 2 - target;
    assert(diff > 0);
    if( diff > ushort.max) {
        error("Loop body too large.");
    }
    emitByte(diff & 0xff);
    emitByte((diff >> 8) & 0xff);
}

private void patchJump(int loc) {
    int amount = (currentChunk.count - (loc + 2));
    if(amount > ushort.max) {
        error("Too much code to jump over.");
    }
    currentChunk.code()[loc] = cast(ubyte)(amount & 0xff);
    currentChunk.code()[loc+1] = cast(ubyte)((amount >> 8) & 0xff);
}

private void block() {
    while (!check(TokenType.RIGHT_BRACE) && !check(TokenType.EOF)) {
        declaration();
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
}

private void beginScope() {
    current.scopeDepth++;
}

private void endScope() {
    assert(current.scopeDepth != 0);
    current.scopeDepth--;

    while (current.localCount > 0 && 
            current.locals[current.localCount - 1].depth >
            current.scopeDepth) {
        if(current.locals[current.localCount - 1].isCaptured)
            emitByte(OpCode.CLOSE_UPVALUE);
        else
            emitByte(OpCode.POP);
        current.localCount--;
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
  TokenType.LEFT_PAREN:    ParseRule(&grouping, &call,   Precedence.CALL),
  TokenType.RIGHT_PAREN:   ParseRule(null,      null,    Precedence.NONE),
  TokenType.LEFT_BRACE:    ParseRule(null,      null,    Precedence.NONE), 
  TokenType.RIGHT_BRACE:   ParseRule(null,      null,    Precedence.NONE),
  TokenType.COMMA:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.DOT:           ParseRule(null,      &dot,    Precedence.CALL),
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
  TokenType.AND:           ParseRule(null,      &and_,   Precedence.AND),
  TokenType.CLASS:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.ELSE:          ParseRule(null,      null,    Precedence.NONE),
  TokenType.FALSE:         ParseRule(&literal,  null,    Precedence.NONE),
  TokenType.FOR:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.FUN:           ParseRule(null,      null,    Precedence.NONE),
  TokenType.IF:            ParseRule(null,      null,    Precedence.NONE),
  TokenType.NIL:           ParseRule(&literal,  null,    Precedence.NONE),
  TokenType.OR:            ParseRule(null,      &or_,    Precedence.OR),
  TokenType.PRINT:         ParseRule(null,      null,    Precedence.NONE),
  TokenType.RETURN:        ParseRule(null,      null,    Precedence.NONE),
  TokenType.SUPER:         ParseRule(&super_,   null,    Precedence.NONE),
  TokenType.THIS:          ParseRule(&this_,    null,    Precedence.NONE),
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

private void super_(bool canAssign) {
    if (currentClass is null) {
        error("Can't use 'super' outside of a class.");
    } else if (!currentClass.hasSuperclass) {
        error("Can't use 'super' in a class with no superclass.");
    }

    consume(TokenType.DOT, "Expect '.' after 'super'.");
    consume(TokenType.IDENTIFIER, "Expect superclass method name.");
    ubyte name = identifierConstant(&parser.previous);

    namedVariable(thisToken, false);
    if (match(TokenType.LEFT_PAREN)) {
        ubyte argCount = argumentList();
        namedVariable(superToken, false);
        emitBytes(OpCode.SUPER_INVOKE, name);
        emitByte(argCount);
    } else {
        namedVariable(superToken, false);
        emitBytes(OpCode.GET_SUPER, name);
    }
}

private void this_(bool canAssign) {
    if (currentClass is null) {
        error("Can't use 'this' outside of a class.");
        return;
    }

    variable(false);
}

private void namedVariable(Token name, bool canAssign) {
    OpCode getOp, setOp;
    int arg = resolveLocal(current, &name);
    if(arg != -1) {
        getOp = OpCode.GET_LOCAL;
        setOp = OpCode.SET_LOCAL;
    } else if ((arg = resolveUpvalue(current, &name)) != -1) {
        getOp = OpCode.GET_UPVALUE;
        setOp = OpCode.SET_UPVALUE;
    } else {
        getOp = OpCode.GET_GLOBAL;
        setOp = OpCode.SET_GLOBAL;
        arg = identifierConstant(&name);
    }
    if (canAssign && match(TokenType.EQUAL)) {
        expression();
        emitBytes(setOp, cast(ubyte)arg);
    } else {
        emitBytes(getOp, cast(ubyte)arg);
    }
}

private int resolveLocal(Compiler* comp, Token *name)
{
    foreach_reverse(i; 0 .. comp.localCount) {
        if(comp.locals[i].name.lexeme == name.lexeme) {
            if(comp.locals[i].depth == -1) {
                error("Can't read local variable in its own initializer.");
            }
            return i;
        }
    }
    return -1;
}

private int resolveUpvalue(Compiler* compiler, Token* name)
{
    if(compiler.enclosing == null) return -1;

    int local = resolveLocal(compiler.enclosing, name);
    if(local != -1) {
        import std.stdio;
        compiler.enclosing.locals[local].isCaptured = true;
        return addUpvalue(compiler, cast(ubyte)local, true);
    }

    int upvalue = resolveUpvalue(compiler.enclosing, name);
    if(upvalue != -1) {
        return addUpvalue(compiler, cast(ubyte)upvalue, false);
    }

    return -1;
}

private int addUpvalue(Compiler* compiler, ubyte index, bool isLocal)
{
    auto upvalueCount = compiler.fun.upvalueCount;

    foreach(i, ref uv; compiler.upvalues[0 .. upvalueCount])
    {
        if(uv.index == index && uv.isLocal == isLocal)
            return cast(int)i;
    }

    if(upvalueCount == ubyte.max + 1)
    {
        error("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues[upvalueCount] =
        Upvalue(
                isLocal: isLocal,
                index: index
               );

    return compiler.fun.upvalueCount++;
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
