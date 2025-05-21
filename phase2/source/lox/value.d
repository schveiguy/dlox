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

struct String {
    string value;
    StringRec* weakRef;
    alias value this;
    ~this() {
        import lox.vm;
        weakRef._refstr = 0;
        vm.garbageStrings = true;
    }
}

struct StringRec {
    size_t _refstr; // obfuscated reference to a String
    this(String* val) {
        _refstr = ~cast(size_t)val;
    }
    String* str() {
        return cast(String*)~_refstr;
    }
    bool isGarbage() => _refstr == 0;
}

alias Value = SumType!(typeof(null), double, bool, String*, ObjFunction*, ObjNative*, ObjClosure*, ObjUpvalue*, ObjClass*, ObjInstance*, ErrStr);

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

    static void printFunction(ObjFunction* f) {
        if(f.name.length > 0)
            outStream.write(i"<fn $(f.name.value)>", false);
        else
            outStream.write(i"<script>", false);
    }

    static void printNative(ObjNative* f) {
        outStream.write(i"<native $(f.name.value)>");
    }

    static void printInstance(ObjInstance* i) {
        outStream.write(i"$(i.klass.name.value) instance");
    }

    value.match!(
            (typeof(null) n) { outStream.write("nil"); },
            (ObjFunction* f) { printFunction(f); },
            (ObjNative* n)   { printNative(n); },
            (ObjClosure* c)  { printFunction(c.fun); },
            (ObjUpvalue* u)  { outStream.write("upvalue"); },
            (ObjClass* c)    { outStream.write(c.name.value); },
            (ObjInstance* i) { printInstance(i); },
            (String* s)      { outStream.write(s.value); },
            (x) { outStream.write(i"$(x)", false); }
    );
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
            (String* s1, String* s2) => Value(internString(s1.value ~ s2.value)),
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

String* extractString(Value v)
{
    // must be a String type
    return v.match!(
            (String* s) => s,
            x => assert(0, "Somehow tried to extract a string from a non string value.")
    );
}

ObjFunction* extractFunction(Value v)
{
    return v.match!(
            (ObjFunction* f) => f,
            (x) => null
    );
}

ObjInstance* extractInstance(Value v)
{
    return v.match!(
            (ObjInstance* i) => i,
            (x) => null
    );
}

String* internString(string s)
{
    import lox.vm;
    if(vm.garbageStrings) {
        vm.garbageStrings = false;
        import core.memory;
        // do not run any GC cycle while cleaning up this garbage
        GC.disable();
        scope(exit) GC.enable();
        StringRec[string] newStrings;
        foreach(k, v; vm.strings) {
            if(!v.isGarbage)
            {
                auto str = v.str;
                newStrings[k] = v;
                // point at the new weak reference
                str.weakRef = &newStrings[k];
            }
        }
        vm.strings = newStrings;
    }

    assert(!vm.garbageStrings);

    if(auto interned = s in vm.strings)
        // already interned.
        return interned.str;
    // intern it
    auto obj = new String(s);
    vm.strings[s] = StringRec(obj);
    obj.weakRef = &vm.strings[s];
    return obj;
}
