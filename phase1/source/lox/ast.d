module lox.ast;
import lox.token;
import std.variant;

// the D way ;)
private string genStuff(T, string VisitorReturn)()
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

    return ctorp ~ ctorb ~ " override " ~ VisitorReturn ~ " acceptHook(Visitor!(typeof(super), " ~ VisitorReturn ~ ") visitor) => visitor.visit(this);";
}

///// EXPRESSIONS

abstract class Expr {
    // hehe, have to be java-like for this one.
    abstract Variant acceptHook(Visitor!(Expr, Variant) visitor);
}

class Binary : Expr {
    Expr left;
    Token operator;
    Expr right;
    mixin(genStuff!(typeof(this), "Variant"));
}

class Grouping : Expr {
    Expr expression;
    mixin(genStuff!(typeof(this), "Variant"));
}

class Literal : Expr {
    Value value;
    mixin(genStuff!(typeof(this), "Variant"));
}

class Unary : Expr {
    Token operator;
    Expr right;
    mixin(genStuff!(typeof(this), "Variant"));
}

class Variable : Expr {
    Token name;
    mixin(genStuff!(typeof(this), "Variant"));
}

class Assign : Expr {
    Token name;
    Expr value;
    mixin(genStuff!(typeof(this), "Variant"));
}

class Logical : Expr {
    Expr left;
    Token operator;
    Expr right;
    mixin(genStuff!(typeof(this), "Variant"));
}

///// STATEMENTS

abstract class Stmt {
    abstract void acceptHook(Visitor!(Stmt, void) visitor);
}

class Expression : Stmt {
    Expr expression;
    mixin(genStuff!(typeof(this), "void"));
}

class Print : Stmt {
    Expr expression;
    mixin(genStuff!(typeof(this), "void"));
}

class Var : Stmt {
    Token name;
    Expr initializer;
    mixin(genStuff!(typeof(this), "void"));
}

class Block : Stmt {
    Stmt[] statements;
    mixin(genStuff!(typeof(this), "void"));
}

class If : Stmt {
    Expr condition;
    Stmt thenBranch;
    Stmt elseBranch;
    mixin(genStuff!(typeof(this), "void"));
}

class While : Stmt {
    Expr condition;
    Stmt body;
    mixin(genStuff!(typeof(this), "void"));
}

class For : Stmt {
    Stmt declaration;
    Expr condition;
    Expr increment;
    Stmt body;
    mixin(genStuff!(typeof(this), "void"));
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
// this works by calling the real visitor, then wrapping it in a variant, and
// unwrapping once it comes out.
R accept(Base, R)(Base item, Visitor!(Base, R) visitor)
{
    static if(is(R == void))
        // void returns do not need a variant intermediate
        return item.acceptHook(visitor);
    else {
        static class VariantVisitor : Visitor!(Base, Variant) {
            Visitor!(Base, R) realVisitor;
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
                    Variant visit(__traits(getMember, mod, mem) item) {
                        return Variant(realVisitor.visit(item));
                    }
                }
            }
        }

        scope vv = new VariantVisitor(visitor);
        // call the variant visitor, then unwrap it.
        return item.acceptHook(vv).get!R;
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
    }
    auto l = new Literal(Value(null));
    auto t = new Token(TokenType.MINUS, "-", Value(null), 0);
    auto u = new Unary(t, l);
    auto b = new Binary(l, t, l);
    auto g = new Grouping(l);
    auto v = new Variable(new Token(TokenType.IDENTIFIER, "a", Value(null), 0));
    auto a = new Assign(new Token(TokenType.IDENTIFIER, "a", Value(null), 0), l);
    auto log = new Logical(l, new Token(TokenType.OR, "or", Value(null), 0), l);
    assert(b.accept(new intVisitor) == 0);
    assert(g.accept(new intVisitor) == 1);
    assert(l.accept(new intVisitor) == 2);
    assert(u.accept(new intVisitor) == 3);
    assert(v.accept(new intVisitor) == 4);
    assert(a.accept(new intVisitor) == 5);
    assert(log.accept(new intVisitor) == 6);
}
