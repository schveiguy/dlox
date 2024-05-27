module lox.interpreter;

import lox.ast;
import lox.token;
import lox.io;

import std.sumtype : match;

class Environment {
    Value[const(char)[]] values;
    Environment enclosing;

    this(Environment enclosing = null) {
        this.enclosing = enclosing;
    }

    ref Value get(Token name) {
        if(auto v = name.lexeme in values) {
            return *v;
        }
        if(enclosing !is null)
            return enclosing.get(name);

        throw new RuntimeException(name,
                "Undefined variable '" ~ cast(string)name.lexeme ~ "'.");
    }

    void define(const(char)[] name, Value value) {
        values[name] = value;
    }
}

class Interpreter : Visitor!(Expr, Value), Visitor!(Stmt, void) {
    Environment environment;

    this()
    {
        environment = new Environment;
    }

    // visitors
    public Value visit(Literal expr) {
        return expr.value;
    }

    public void interpret(Stmt[] statements) {
        try {
            foreach(stmt; statements)
                execute(stmt);
        } catch (RuntimeException error) {
            import lox.lox;
            runtimeError(error);
        }
    }

    public Value visit(Grouping expr) {
        return evaluate(expr.expression);
    }

    public Value visit(Unary expr) {
        auto val = evaluate(expr.right);
        // must use double for negation. Just accepting a double means it will accept bools
        static Value negDouble(T)(T v) if(is(T == double)) => Value(-v);
        with(TokenType) switch(expr.operator.type)
        {
            case MINUS:
                return val.match!(
                        negDouble,
                        _ => throw new RuntimeException(expr.operator, "Operand must be an number.")
                        );
            case BANG:
                return Value(!val.isTruthy);
            default:
                assert(false, "Internal error");
        }
    }

    public Value visit(Binary expr) {
        auto leftVal = evaluate(expr.left);
        auto rightVal = evaluate(expr.right);

        template mustBeNums(string op) {
            static Value mustBeNums(T, U)(T l, U r) if(is(T == double) && is(U == double))
            => mixin("Value(l", op, "r)");
        }
        Value numberOp(string op)() {
            return match!(
                    mustBeNums!op,
                    (_1, _2) => throw new RuntimeException(expr.operator, "Operands must be numbers.")
                  )(leftVal, rightVal);
        }

        static Value equalVals(T, U)(T t1, U t2) if(is(T == U)){
            static if(is(T == double))
                return Value(t1 is t2);
            else
                return Value(t1 == t2);
        }
        static Value notEqualVals(T, U)(T t1, U t2) if(is(T == U)){
            static if(is(T == double))
                return Value(t1 !is t2);
            else
                return Value(t1 != t2);
        }
        with(TokenType) switch(expr.operator.type)
        {
            case PLUS:
                return match!(
                        mustBeNums!"+",
                        (const(char)[] l, const(char)[] r) => Value(l ~ r),
                        (_1, _2) => throw new RuntimeException(expr.operator, "Operands must be two numbers or two strings.")
                        )(leftVal, rightVal);
            case MINUS:
                return numberOp!"-";
            case SLASH:
                return numberOp!"/";
            case STAR:
                return numberOp!"*";
            case GREATER:
                return numberOp!">";
            case LESS:
                return numberOp!"<";
            case GREATER_EQUAL:
                return numberOp!">=";
            case LESS_EQUAL:
                return numberOp!"<=";
            case EQUAL_EQUAL:
                return match!(equalVals,
                        (_1, _2) => Value(false)
                        )(leftVal, rightVal);
            case BANG_EQUAL:
                return match!(notEqualVals,
                        (_1, _2) => Value(true)
                        )(leftVal, rightVal);
            default:
                assert(0, "Internal error");
        }
    }

    public Value visit(Variable expr) {
        return environment.get(expr.name);
    }

    Value visit(Assign expr) {
        auto val = evaluate(expr.value);
        environment.get(expr.name) = val;
        return val;
    }

    Value visit(Logical expr) {
        auto val = evaluate(expr.left);
        if(expr.operator.type == TokenType.OR && val.isTruthy)
            return val;
        else if(expr.operator.type == TokenType.AND && !val.isTruthy)
            return val;
        return evaluate(expr.right);
    }

    void visit(Expression stmt) {
        auto val = evaluate(stmt.expression);
    }

    void visit(Print stmt) {
        auto val = evaluate(stmt.expression);
        import lox.io;
        outStream.writeln(val.toString());
    }

    void visit(Var stmt) {
        Value val = null;
        if(stmt.initializer !is null)
        {
            val = evaluate(stmt.initializer);
        }

        environment.define(stmt.name.lexeme, val);
    }

    void visit(Block stmt) {
        executeBlock(stmt.statements, new Environment(environment));
    }

    void visit(If stmt) {
        auto condResult = evaluate(stmt.condition);
        if(condResult.isTruthy)
            execute(stmt.thenBranch);
        else if(stmt.elseBranch !is null)
            execute(stmt.elseBranch);
    }

    void visit(While stmt) {
        while(evaluate(stmt.condition).isTruthy)
        {
            execute(stmt.body);
        }
    }

    void visit(For stmt) {
        auto previous = this.environment;
        this.environment = new Environment(previous);
        scope(exit) this.environment = previous;
        if(stmt.declaration !is null)
            execute(stmt.declaration);
        while(stmt.condition is null || evaluate(stmt.condition).isTruthy)
        {
            execute(stmt.body);
            if(stmt.increment !is null)
                evaluate(stmt.increment);
        }
    }

    private void executeBlock(Stmt[] statements, Environment environment)
    {
        auto previous = this.environment;
        this.environment = environment;
        scope(exit) this.environment = previous;

        foreach(stmt; statements)
        {
            execute(stmt);
        }
    }

    // helper
    private Value evaluate(Expr expr) {
        return expr.accept(this);
    }

    private void execute(Stmt stmt) {
        return stmt.accept(this);
    }
}

class RuntimeException : Exception {
    const Token token;
    this(Token tok, string message, string file = __FILE__, size_t line = __LINE__)
    {
        this.token = tok;
        super(message, file, line);
    }
}

bool isTruthy(Value v)
{
    return v.match!(
            (bool b) => b,
            (typeof(null) n) => false,
            _ => true
            );
}
