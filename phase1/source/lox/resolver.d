module lox.resolver;

import lox.ast;
import lox.token;
//import lox.interpreter;

final class Resolver : Visitor!(Expr, void), Visitor!(Stmt, void) {
    //private Interpreter interpreter;
    private bool[const(char)[]][] scopes;
    private enum FunctionType {
        NONE,
        FUNCTION,
        INITIALIZER,
        METHOD
    }

    private enum ClassType {
        NONE,
        CLASS
    }

    private FunctionType currentFunction = FunctionType.NONE;
    private ClassType currentClass = ClassType.NONE;

    void visit(Block stmt) {
        beginScope();
        resolve(stmt.statements);
        endScope();
    }

    void visit(Var stmt) {
        declare(stmt.name);
        if (stmt.initializer !is null) {
            resolve(stmt.initializer);
        }
        define(stmt.name);
    }

    void visit(Variable expr) {
        // note, we get with a default value of true, because we are explicitly
        // looking for false here.
        if(scopes.length && !scopes[$-1].get(expr.name.lexeme, true))
        {
            import lox.lox;
            error(expr.name, "Can't read local variable in its own initializer.");
        }

        resolveLocal(expr, expr.name);
    }

    void visit(Assign expr) {
        resolve(expr.value);
        resolveLocal(expr, expr.name);
    }

    void visit(Function stmt) {
        declare(stmt.name);
        define(stmt.name);

        resolveFunction(stmt, FunctionType.FUNCTION);
    }

    void visit(Expression stmt) => resolve(stmt.expression);
    void visit(If stmt) {
        resolve(stmt.condition);
        resolve(stmt.thenBranch);
        if(stmt.elseBranch !is null) resolve(stmt.elseBranch);
    }
    void visit(Print stmt) => resolve(stmt.expression);

    void visit(Return stmt) {
        import lox.lox;
        if(currentFunction == FunctionType.NONE)
            error(stmt.keyword, "Can't return from top-level code.");

        if(stmt.value !is null)
        {
            if(currentFunction == FunctionType.INITIALIZER)
                error(stmt.keyword, "Can't return a value from an initializer.");
            resolve(stmt.value);
        }
    }

    void visit(While stmt) {
        resolve(stmt.condition);
        resolve(stmt.body);
    }

    void visit(For stmt) {
        if(stmt.declaration !is null)
            resolve(stmt.declaration);
        if(stmt.condition !is null)
            resolve(stmt.condition);
        if(stmt.increment !is null)
            resolve(stmt.increment);
        resolve(stmt.body);
    }

    void visit(Class stmt) {
        ClassType enclosingClass = currentClass;
        scope(exit) currentClass = enclosingClass;
        currentClass = ClassType.CLASS;

        declare(stmt.name);
        define(stmt.name);

        beginScope();
        scope(exit) endScope();
        scopes[$-1]["this"] = true;

        foreach(method; stmt.methods) {
            resolveFunction(method, method.name.lexeme == "init" ? FunctionType.INITIALIZER : FunctionType.METHOD);
        }
    }

    void visit(Binary expr) {
        resolve(expr.left);
        resolve(expr.right);
    }

    void visit(Call expr) {
        resolve(expr.callee);
        foreach(argument; expr.arguments)
            resolve(argument);
    }

    void visit(Get expr) => resolve(expr.obj);

    void visit(Set expr) {
        resolve(expr.obj);
        resolve(expr.value);
    }

    void visit(This expr) {
        if(currentClass == ClassType.NONE)
        {
            import lox.lox;
            error(expr.keyword, "Can't use 'this' outside a class.");
            return;
        }
        resolveLocal(expr, expr.keyword);
    }

    void visit(Grouping expr) => resolve(expr.expression);

    void visit(Literal expr) {}

    void visit(Logical expr) {
        resolve(expr.left);
        resolve(expr.right);
    }

    void visit(Unary expr) => resolve(expr.right);

    void resolve(Stmt[] statements) {
        foreach(stmt; statements) {
            resolve(stmt);
        }
    }

    private void resolve(Stmt stmt) {
        stmt.accept(this);
    }

    private void resolve(Expr expr) {
        expr.accept(this);
    }

    private void beginScope() {
        scopes.length += 1;
    }

    private void endScope() {
        scopes = scopes[0 .. $-1];
        scopes.assumeSafeAppend;
    }

    private void declare(Token name) {
        if(scopes.length == 0) return;

        import lox.lox;
        scopes[$-1].update(name.lexeme,
                () => false,
                (bool val) {
                    error(name, "Already a variable with this name in this scope.");
                    return false;
                });
    }

    private void define(Token name) {
        if(scopes.length == 0) return;
        scopes[$-1][name.lexeme] = true;
    }

    private void resolveLocal(Expr expr, Token name) {
        foreach_reverse(i, ref sc; scopes) {
            if(name.lexeme in sc) {
                expr.localScope = scopes.length - 1 - i;
                return;
            }
        }
    }

    private void resolveFunction(Function stmt, FunctionType type) {
        auto enclosingFunction = currentFunction;
        scope(exit) currentFunction = enclosingFunction;
        currentFunction = type;
        beginScope();
        foreach(param; stmt.params) {
            declare(param);
            define(param);
        }

        resolve(stmt.body);
        endScope();
    }
}
