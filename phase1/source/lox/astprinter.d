module lox.astprinter;
import lox.ast;

class AstPrinter : Visitor!(Expr, string) {
    string print(Expr expr) {
        return expr.accept(this);
    }

    public string visit(Binary expr) {
        return parenthesize(expr.operator.lexeme,
                expr.left, expr.right);
    }

    public string visit(Grouping expr) {
        return parenthesize("group", expr.expression);
    }

    public string visit(Literal expr) {
        import std.sumtype;
        if (expr.value.match!(
                    (typeof(null) n) => true,
                    t => false
                    ))
            return "nil";
        return expr.value.toString();
    }

    public string visit(Unary expr) {
        return parenthesize(expr.operator.lexeme, expr.right);
    }

    public string visit(Variable expr) {
        return parenthesize("var " ~ expr.name.lexeme);
    }

    public string visit(Assign expr) {
        return parenthesize("assign " ~ expr.name.lexeme, expr.value);
    }

    public string visit(Logical expr) {
        return parenthesize(expr.operator.lexeme, expr.left, expr.right);
    }

    public string visit(Call expr) {
        return parenthesize("call " ~ expr.callee.accept(this), expr.arguments);
    }

    public string visit(Get expr) {
        return expr.obj.accept(this) ~ "." ~ cast(string)expr.name.lexeme;
    }
    public string visit(Set expr) {
        return parenthesize("set ", expr.obj, expr.value);
    }

    public string visit(This expr) {
        return "this";
    }

    public string visit(Super expr) {
        return "super." ~ cast(string)expr.method.lexeme;
    }

    private string parenthesize(const(char)[] name, Expr[] exprs...) {
        import std.array : Appender;
        Appender!string app;
        app.put("(");
        app.put(name);
        foreach(e; exprs) {
            app.put(" ");
            app.put(e.accept(this));
        }
        app.put(")");
        return app.data;
    }
}

unittest {
    import lox.token;
    Expr expression = new Binary(
        new Unary(
            new Token(TokenType.MINUS, "-", Value(null), 1),
            new Literal(Value(123))),
        new Token(TokenType.STAR, "*", Value(null), 1),
        new Grouping(
            new Literal(Value(45.67))));

    //import std.stdio;
    //writeln(new AstPrinter().print(expression));
    assert(new AstPrinter().print(expression) == "(* (- 123) (group 45.67))");
}
