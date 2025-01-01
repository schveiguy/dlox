module lox.value;
import lox.obj;

import std.sumtype; // going to use this instead of custom-built tagged union

/*struct Value {
    private alias ST = SumType!(double, bool);
    ST _val;

    this(T)(T val)
    {
        static if(is(T == Value))
            _val = val._val;
        else
            _val = ST(val);
    }

    ref Value opAssign(T)(T val) if (__traits(compiles, _val = val)) {
        _val = val;
        return this;
    }

}*/

struct ErrStr {
    string msg;
}

alias Value = SumType!(typeof(null), double, bool, string, Obj*, ErrStr);

struct ValueArray {
    Value[] _storage;
    int count;
    inout(Value[]) values() inout {
        return _storage[0 .. count];
    }
    
    void write(Value val) {
        if(count == _storage.length)
        {
            // utilize appending amortized growth.
            _storage ~= val;
            ++count;
        }
        else
            _storage[count++] = val;
    }
}

void printValue(Value value) {
    import lox.io;
    outStream.write(i"$(value)");
}

Value negate(Value v)
{
    return v.match!(
            (double d) => Value(-d),
            (x) => Value(ErrStr("Operand must be a number."))
    );
}

bool asBool(Value v)
{
    // nil and false are considered false, and every other value is considered true (even 0)
    return v.match!(
            (bool b) => b,
            (typeof(null) n) => false,
            (x) => true 
    );
}

Value numOp(string op)(Value v1, Value v2)
{
    static Value binop(T, U)(T a, U b) if (is(T == double) && is(U == double))
    {
        return Value(mixin("a ", op, " b"));
    }

    return match!(
            binop,
            (a, b) => Value(ErrStr("Operands must be numbers."))
    )(v1, v2);
}

Value addValues(Value v1, Value v2)
{
    static Value numadd(T, U)(T a, U b) if (is(T == double) && is(U == double))
    {
        return Value(a + b);
    }

    return match!(
            (string s1, string s2) => Value(internString(s1 ~ s2)),
            numadd,
            (a, b) => Value(ErrStr("Operands must be two numbers or two strings."))
    )(v1, v2);
}

Value valueEqual(Value v1, Value v2)
{
    return Value(match!(
            (a, b) {
                static if(is(typeof(a) == typeof(b)))
                    return a == b;
                else
                    return false;
                    }
    )(v1, v2));
}

string getError(Value v)
{
    return v.match!(
            (ErrStr e) => e.msg,
            x => null
    );
}

string extractString(Value v)
{
    // must be a string type
    return v.match!(
            (string s) => s,
            x => assert(0, "Somehow tried to extract a string from a non string value.")
    );
}

string internString(string s)
{
    import lox.vm;
    if(auto interned = s in vm.strings)
        // already interned.
        return *interned;
    // intern it
    vm.strings[s] = s;
    return s;
}
