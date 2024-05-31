module lox.token;

import std.sumtype;

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
    LoxFunction[const(char)[]] methods;

    this(const(char)[] name, typeof(methods) methods) {
        this.name = name;
        this.methods = methods;
        if(auto initializer = "init" in methods)
            initializer.isInitializer = true;
    }

    override string toString() {
        return "<class " ~ cast(string)name ~ ">";
    }

    LoxFunction *findMethod(const(char)[] name) {
        return name in methods;
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

enum TokenType {
  // Single-character tokens.
  LEFT_PAREN, RIGHT_PAREN, LEFT_BRACE, RIGHT_BRACE,
  COMMA, DOT, MINUS, PLUS, SEMICOLON, SLASH, STAR,

  // One or two character tokens.
  BANG, BANG_EQUAL,
  EQUAL, EQUAL_EQUAL,
  GREATER, GREATER_EQUAL,
  LESS, LESS_EQUAL,

  // Literals.
  IDENTIFIER, STRING, NUMBER,

  // Keywords.
  AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR,
  PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE,

  EOF
}

class Token {
    const {
        TokenType type;
        char[] lexeme;
        Value literal;
        int line;
    }

    this(TokenType type, const(char)[] lexeme, Value literal, int line) {
        this.type = type;
        this.lexeme = lexeme;
        this.literal = literal;
        this.line = line;
    }

    override public string toString() const {
        import std.conv;
        return text(type, " ", lexeme, " ", literal);
    }
}
