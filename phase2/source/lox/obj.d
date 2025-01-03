module lox.obj;

import lox.chunk;
import lox.value;

struct ObjFunction {
    int arity;
    Chunk chunk;
    string name;
}

alias NativeFn = Value function(Value[] args);

struct ObjNative {
    NativeFn fun;
    string name;
}

T convParamTest(T, U)(U v) {
    static if(!__traits(compiles, convParam!T(v)))
        pragma(msg, "T is ", T, "U is ", U);
    return convParam!T(v);
}

T convParam(T, U)(U v) {
    import std.conv;
    static if(is(U == ObjFunction*) || is(U == ObjNative*))
        // never convertible
        throw new Exception("Incompatible parameter types: '" ~ U.stringof ~ "' and '" ~ T.stringof ~ "'");
    else static if(is(U == ErrStr))
        assert(0, "ErrStr in arguments!");
    else static if(__traits(compiles, v.to!T)) return v.to!T;
    else throw new Exception("Incompatible parameter types: '" ~ U.stringof ~ "' and '" ~ T.stringof ~ "'");
}

ObjNative* getNativeFn(alias symbol)() {
    import std.traits;
    import std.conv;
    import std.sumtype;
    static ObjNative result;
    static Value wrapper(Value[] args) {
        enum argCountExpected = arity!symbol;
        if(args.length != argCountExpected)
            return Value(ErrStr(i"Invalid argument count for function '$(__traits(identifier, symbol))'. Expected $(argCountExpected) but received $(args.length).".text));

        // convert all the args to the proper arguments
        Parameters!symbol nativeArgs;
        static foreach(i; 0 .. nativeArgs.length) {
            try {
                alias T = typeof(nativeArgs[i]);
                nativeArgs[i] = args[i].match!(
                        (x) => x.convParam!T
                );
            }
            catch(Exception e) {
                return Value(ErrStr("NATIVE ARG ERROR: " ~ e.msg));
            }
        }

        static if(is(typeof(symbol(nativeArgs)) == void))
        {
            symbol(nativeArgs);
            return Value(null); // functions that have no return type return Nil.
        }
        else
        {
            auto result = symbol(nativeArgs);
            return Value(result);
        }
    }

    if(result.fun is null)
    {
        // initialize it
        result.fun = &wrapper;
        result.name = __traits(identifier, symbol);
    }

    return &result;
}
