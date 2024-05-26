module lox.ast;
import lox.token;
import std.variant;

abstract class Expr {
    // hehe, have to be java-like for this one.
    abstract Variant acceptVariant(Visitor!Variant visitor);
}

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

    return ctorp ~ ctorb ~ " override Variant acceptVariant(Visitor!Variant visitor) => visitor.visit(this);";
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

// implement the visitor stuff

private alias mod = mixin(__MODULE__);

// create the visitor interface, we gonna use overloading...
interface Visitor(R) {
    static foreach(mem; __traits(allMembers, mod))
    {
        static if(is(__traits(getMember, mod, mem) == Expr))
        {
            // skip the actual Expr class
        }
        else static if(is(__traits(getMember, mod, mem) : Expr))
        {
            R visit(__traits(getMember, mod, mem) expr);
        }
    }
}

// the UFCS accept method (cannot be virtual).
// this works by calling the real visitor, then wrapping it in a variant, and
// unwrapping once it comes out.
R accept(R)(Expr expr, Visitor!R visitor)
{
    static class VariantVisitor : Visitor!Variant {
        Visitor!R realVisitor;
        this(Visitor!R rv) {
            this.realVisitor = rv;
        }

        static foreach(mem; __traits(allMembers, mod))
        {
            static if(is(__traits(getMember, mod, mem) == Expr))
            {
                // skip the actual Expr class
            }
            else static if(is(__traits(getMember, mod, mem) : Expr))
            {
                Variant visit(__traits(getMember, mod, mem) expr) {
                    return Variant(realVisitor.visit(expr));
                }
            }
        }
    }

    scope vv = new VariantVisitor(visitor);
    // call the variant visitor, then unwrap it.
    return expr.acceptVariant(vv).get!R;
}

// keep this up to date with all the types
unittest {
    static class intVisitor : Visitor!int {
        int visit(Binary) => 0;
        int visit(Grouping) => 1;
        int visit(Literal) => 2;
        int visit(Unary) => 3;
    }
    auto l = new Literal(Value(null));
    auto t = new Token(TokenType.MINUS, "-", Value(null), 0);
    auto u = new Unary(t, l);
    auto b = new Binary(l, t, l);
    auto g = new Grouping(l);
    assert(b.accept(new intVisitor) == 0);
    assert(g.accept(new intVisitor) == 1);
    assert(l.accept(new intVisitor) == 2);
    assert(u.accept(new intVisitor) == 3);
}
