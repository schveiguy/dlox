module lox.value;

import lox.token;

import std.sumtype;

public import std.sumtype : match;

struct LoxFunction
{
    private import lox.ast : Stmt;
    private import lox.interpreter : Environment;
    const(char)[] name;
    const(char[])[] parameters;
    Stmt[] statements;
    Value function(Value[]) nativeFn;
    Environment closure;
    bool isInitializer = false;
    string toString() {
        return (nativeFn ? "<nativeFn " : "<fn ") ~ cast(string)name ~ ">";
    }

    LoxFunction bind(LoxInstance instance) {
        Environment environment = new Environment(closure);
        environment.define("this", Value(instance));
        return LoxFunction(name: name, parameters: parameters, statements: statements, closure: environment);
    }
}

class LoxClass
{
    const(char)[] name;
    LoxClass superclass;
    LoxFunction[const(char)[]] methods;

    this(const(char)[] name, LoxClass superclass, typeof(methods) methods) {
        this.name = name;
        this.superclass = superclass;
        this.methods = methods;
        if(auto initializer = "init" in methods)
            initializer.isInitializer = true;
    }

    override string toString() {
        return "<class " ~ cast(string)name ~ ">";
    }

    LoxFunction *findMethod(const(char)[] name) {
        if(auto n = name in methods)
            return n;
        if(superclass !is null)
            return superclass.findMethod(name);
        return null;
    }
}

struct LoxInstance {
    private LoxClass klass;

    Value[const(char)[]] properties;

    this(LoxClass klass) {
        this.klass = klass;
        this.properties = new typeof(properties);
    }

    Value get(Token name) {
        import lox.interpreter : RuntimeException;
        if(auto v = name.lexeme in properties)
            return *v;

        if(auto m = klass.findMethod(name.lexeme))
        {
            return Value(m.bind(this));
        }

        throw new RuntimeException(name, "Undefined property '" ~ cast(string)name.lexeme ~ "'.");
    }

    Value set(Token name, Value v)
    {
        return properties[name.lexeme] = v;
    }

    string toString() {
        return "<class " ~ cast(string)klass.name ~ " instance>";
    }
}

// substitute for Java Object for literals
alias Value = SumType!(double, const(char)[], bool, typeof(null), LoxFunction, LoxClass, LoxInstance);

