module lox.interpreter;

import lox.expr;
import lox.token;
import lox.io;

import std.sumtype : match;

class Interpreter : Visitor!Value {
    // visitors
    public Value visit(Literal expr) {
        return expr.value;
    }

    public void interpret(Expr expression) {
        try {
            Value value = evaluate(expression);
            outStream.writeln(value.toString());
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
        with(TokenType) switch(expr.operator.type)
        {
            case MINUS:
                return val.match!(
                        (double v) => Value(-v),
                        _ => throw new RuntimeException(expr.operator, "Operand must be an number.")
                        );
            case BANG:
                return val.match!(
                        (bool b) => Value(!b),
                        (typeof(null) n) => Value(true),
                        _ => Value(false)
                        );
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

    // helper
    private Value evaluate(Expr expr) {
        return expr.accept(this);
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
