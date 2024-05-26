module lox.astprinter;
import lox.ast;

class AstPrinter : Visitor!string {
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
