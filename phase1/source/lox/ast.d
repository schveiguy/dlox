module lox.ast;
import lox.token;
import lox.value;

// the D way ;)
private string genStuff(T)()
{
    string ctorp = "this(";
    string ctorb = "{";
    static foreach(i; 0 .. T.tupleof.length)
    {{
        auto name = T.tupleof[i].stringof;
        ctorp ~= "typeof(this.tupleof[" ~ i.stringof ~ "]) " ~ name ~ ", ";
        ctorb ~= "this." ~ name ~ " = " ~ name ~ ";";
    }}
    ctorp ~= ")";
    ctorb ~= "}";

    return ctorp ~ ctorb ~ " override void acceptHook(Visitor!(typeof(super), void) visitor) => visitor.visit(this);";
}

// Note about visitors:
// D does not allow templated virtual functions. However, the visitor pattern
// *passes the visitor to the derived type*. This means that we have a location
// to store the "true" return type, and we just need to unwrap it on the call
// side. This is done through the UFCS function `accept`. The `acceptHook`
// always returns `void`, because the true return value is in the visitor
// itself. See the `accept` function for more details.

///// EXPRESSIONS

abstract class Expr {
    // semantic info
    ptrdiff_t localScope = -1;
    abstract void acceptHook(Visitor!(Expr, void) visitor);
}

class Binary : Expr {
    Expr left;
    Token operator;
    Expr right;
    mixin(genStuff!(typeof(this)));
}

class Grouping : Expr {
    Expr expression;
    mixin(genStuff!(typeof(this)));
}

class Literal : Expr {
    Value value;
    mixin(genStuff!(typeof(this)));
}

class Unary : Expr {
    Token operator;
    Expr right;
    mixin(genStuff!(typeof(this)));
}

class Variable : Expr {
    Token name;
    mixin(genStuff!(typeof(this)));
}

class Assign : Expr {
    Token name;
    Expr value;
    mixin(genStuff!(typeof(this)));
}

class Logical : Expr {
    Expr left;
    Token operator;
    Expr right;
    mixin(genStuff!(typeof(this)));
}

class Call : Expr {
    Expr callee;
    Token paren;
    Expr[] arguments;
    mixin(genStuff!(typeof(this)));
}

class Get : Expr {
    Expr obj;
    Token name;
    mixin(genStuff!(typeof(this)));
}

class Set : Expr {
    Expr obj;
    Token name;
    Expr value;
    mixin(genStuff!(typeof(this)));
}

class This : Expr {
    Token keyword;
    mixin(genStuff!(typeof(this)));
}

class Super : Expr {
    Token keyword;
    Token method;
    mixin(genStuff!(typeof(this)));
}

///// STATEMENTS

abstract class Stmt {
    abstract void acceptHook(Visitor!(Stmt, void) visitor);
}

class Expression : Stmt {
    Expr expression;
    mixin(genStuff!(typeof(this)));
}

class Print : Stmt {
    Expr expression;
    mixin(genStuff!(typeof(this)));
}

class Var : Stmt {
    Token name;
    Expr initializer;
    mixin(genStuff!(typeof(this)));
}

class Block : Stmt {
    Stmt[] statements;
    mixin(genStuff!(typeof(this)));
}

class If : Stmt {
    Expr condition;
    Stmt thenBranch;
    Stmt elseBranch;
    mixin(genStuff!(typeof(this)));
}

class While : Stmt {
    Expr condition;
    Stmt body;
    mixin(genStuff!(typeof(this)));
}

class For : Stmt {
    Stmt declaration;
    Expr condition;
    Expr increment;
    Stmt body;
    mixin(genStuff!(typeof(this)));
}

class Function : Stmt {
    Token name;
    Token[] params;
    Stmt[] body;
    mixin(genStuff!(typeof(this)));
}

class Return : Stmt {
    Token keyword;
    Expr value;
    mixin(genStuff!(typeof(this)));
}

class Class : Stmt {
    Token name;
    Variable superclass;
    Function[] methods;
    mixin(genStuff!(typeof(this)));
}

///// VISITOR

private alias mod = mixin(__MODULE__);

// create the visitor interface, we gonna use overloading...
interface Visitor(Base, R) {
    static foreach(mem; __traits(allMembers, mod))
    {
        static if(is(__traits(getMember, mod, mem) == Base))
        {
            // skip the actual Base class
        }
        else static if(is(__traits(getMember, mod, mem) : Base))
        {
            R visit(__traits(getMember, mod, mem) item);
        }
    }
}

// the UFCS accept method (cannot be virtual).
// this works by calling the real visitor, storing the return value as a
// member, and returning the value in the outer call.
R accept(Base, R)(Base item, Visitor!(Base, R) visitor)
{
    static if(is(R == void))
        // void returns do not need an intermediate
        return item.acceptHook(visitor);
    else {
        static class WrappingVisitor : Visitor!(Base, void) {
            private Visitor!(Base, R) realVisitor;
            private R returnValue;
            this(Visitor!(Base, R) rv) {
                this.realVisitor = rv;
            }

            static foreach(mem; __traits(allMembers, mod))
            {
                static if(is(__traits(getMember, mod, mem) == Base))
                {
                    // skip the actual Base class
                }
                else static if(is(__traits(getMember, mod, mem) : Base))
                {
                    void visit(__traits(getMember, mod, mem) item) {
                        returnValue = realVisitor.visit(item);
                    }
                }
            }
        }

        scope wrapper = new WrappingVisitor(visitor);
        // call the wrapping visitor, then fetch the return value.
        item.acceptHook(wrapper);
        return wrapper.returnValue;
    }
}

// keep this up to date with all the types
unittest {
    static class intVisitor : Visitor!(Expr, int) {
        int visit(Binary) => 0;
        int visit(Grouping) => 1;
        int visit(Literal) => 2;
        int visit(Unary) => 3;
        int visit(Variable) => 4;
        int visit(Assign) => 5;
        int visit(Logical) => 6;
        int visit(Call) => 7;
        int visit(Get) => 8;
        int visit(Set) => 9;
        int visit(This) => 10;
    }
    auto t = new Token(TokenType.MINUS, "-", Value(null), 0);
    auto id = new Token(TokenType.IDENTIFIER, "a", Value(null), 0);

    auto l = new Literal(Value(null));
    auto u = new Unary(t, l);
    auto b = new Binary(l, t, l);
    auto g = new Grouping(l);
    auto v = new Variable(id);
    auto a = new Assign(id, l);
    auto log = new Logical(l, new Token(TokenType.OR, "or", Value(null), 0), l);
    auto c = new Call(v, new Token(TokenType.RIGHT_PAREN, ")", Value(null), 0), [l]);
    auto get = new Get(v, id);
    auto set = new Set(v, id, l);
    auto ths = new This(new Token(TokenType.THIS, "this", Value(null), 0));
    assert(b.accept(new intVisitor) == 0);
    assert(g.accept(new intVisitor) == 1);
    assert(l.accept(new intVisitor) == 2);
    assert(u.accept(new intVisitor) == 3);
    assert(v.accept(new intVisitor) == 4);
    assert(a.accept(new intVisitor) == 5);
    assert(log.accept(new intVisitor) == 6);
    assert(c.accept(new intVisitor) == 7);
    assert(get.accept(new intVisitor) == 8);
    assert(set.accept(new intVisitor) == 9);
    assert(ths.accept(new intVisitor) == 10);
}
