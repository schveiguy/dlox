module lox.vm;

import lox.chunk;
import lox.io;
import lox.value;
import lox.compiler;
import lox.obj;

enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
}

enum FRAMES_MAX = 64;
enum STACK_MAX = FRAMES_MAX * 256;

struct CallFrame {
    ObjFunction* fun;
    ubyte[] ip;
    Value* slots;
}

struct VM {
    CallFrame[FRAMES_MAX] frames;
    int frameCount;

    Value[STACK_MAX] stack;
    Value* stackTop;
    string[string] strings;
    Value[string] globals;

    InterpretResult run() {
        auto frame = &frames[vm.frameCount - 1];

        ref Value stackLocal(ubyte idx) {
            assert(frame.slots + idx < stackTop);
            return frame.slots[idx];
        }

        ubyte READ_BYTE() {
            scope(success) frame.ip = frame.ip[1 .. $];
            return frame.ip[0];
        }
        ushort READ_SHORT() {
            scope(success) frame.ip = frame.ip[2 .. $];
            return frame.ip[0] | (frame.ip[1] << 8);
        }

        Value READ_CONSTANT() => frame.fun.chunk.constants.values[READ_BYTE()];

        bool BINARY_OP(string op)() {
            auto b = pop();
            auto a = pop();
            auto result = numOp!op(a, b);
            if(auto msg = result.getError()) {
                push(a); // put them back on the stack
                push(b);
                runtimeError("Operands must be numbers.");
                return false;
            }
            push(result);
            return true;
        }

        ptrdiff_t ipIdx() => frame.ip.ptr - frame.fun.chunk.code.ptr;

        for (;;) {
            debug(TRACE_EXECUTION) {
                import lox.dbg;
                auto o = outStream;
                o.write("          ", false);
                foreach (slot; stack[0 .. stackTop - stack.ptr])
                {
                    o.write("[ ", false);
                    printValue(slot);
                    o.write(" ]", false);
                }
                o.writeln();
                disassembleInstruction(frame.fun.chunk, cast(int)(frame.fun.chunk.code.length - frame.ip.length));
            }
            ubyte instruction;
            with(OpCode) final switch(instruction = READ_BYTE()) {
                case CONSTANT:
                    Value constant = READ_CONSTANT();
                    push(constant);
                    break;
                case NIL:
                    push(Value(null));
                    break;
                case TRUE:
                    push(Value(true));
                    break;
                case FALSE:
                    push(Value(false));
                    break;
                case EQUAL:
                    auto b = pop();
                    auto a = pop();
                    push(valueEqual(a, b));
                    break;
                case GREATER:
                    if(!BINARY_OP!">"()) return InterpretResult.RUNTIME_ERROR;
                    break;
                case LESS:
                    if(!BINARY_OP!"<"()) return InterpretResult.RUNTIME_ERROR;
                    break;
                case ADD:
                    // handle add differently, as it needs to deal with strings and numbers.
                    auto b = pop();
                    auto a = pop();
                    auto result = addValues(a, b);
                    if(auto msg = result.getError()) {
                        push(a); // put the values back.
                        push(b);
                        runtimeError(msg);
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    push(result);
                    break;
                case SUBTRACT:
                    if(!BINARY_OP!"-"()) return InterpretResult.RUNTIME_ERROR;
                    break;
                case MULTIPLY:
                    if(!BINARY_OP!"*"()) return InterpretResult.RUNTIME_ERROR;
                    break;
                case DIVIDE:
                    if(!BINARY_OP!"/"()) return InterpretResult.RUNTIME_ERROR;
                    break;
                case NOT:
                    auto val = pop();
                    auto result = Value(!val.asBool());
                    push(result);
                    break;
                case NEGATE:
                    auto val = pop();
                    auto result = val.negate();
                    if(auto msg = result.getError())
                    {
                        push(val); // put it back
                        runtimeError(msg);
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    push(result);
                    break;
                case PRINT:
                    printValue(pop());
                    outStream.writeln();
                    break;
                case POP:
                    pop();
                    break;
                case DEFINE_GLOBAL:
                    auto nameVal = READ_CONSTANT();
                    string name = nameVal.extractString();
                    globals[name] = pop();
                    break;
                case GET_LOCAL:
                    auto idx = READ_BYTE();
                    push(stackLocal(idx));
                    break;
                case SET_LOCAL:
                    auto idx = READ_BYTE();
                    auto val = peek();
                    stackLocal(idx) = val;
                    break;
                case GET_GLOBAL:
                    auto nameVal = READ_CONSTANT();
                    string name = nameVal.extractString();
                    if(auto v = name in globals) {
                        push(*v);
                    } else {
                        import std.conv;
                        runtimeError(i"Undefined variable '$(name)'".text);
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case SET_GLOBAL:
                    auto nameVal = READ_CONSTANT();
                    string name = nameVal.extractString();
                    if(auto v = name in globals) {
                        auto val = peek();
                        *v = val;
                    } else {
                        import std.conv;
                        runtimeError(i"Undefined variable '$(name)'".text);
                        return InterpretResult.RUNTIME_ERROR;
                    }
                    break;
                case JUMP_IF_FALSE:
                    auto offset = READ_SHORT();
                    auto val = peek();
                    if (!val.asBool) {
                        auto idx = ipIdx();
                        idx += offset;
                        frame.ip = frame.fun.chunk.code[idx .. $];
                    }
                    break;
                case JUMP:
                    auto offset = READ_SHORT();
                    auto idx = ipIdx();
                    idx += offset;
                    frame.ip = frame.fun.chunk.code[idx .. $];
                    break;
                case LOOP:
                    auto offset = READ_SHORT();
                    auto idx = ipIdx();
                    idx -= offset;
                    frame.ip = frame.fun.chunk.code[idx .. $];
                    break;
                case CALL:
                    auto argCount = READ_BYTE();
                    auto func = peek(argCount);
                    with(CallableType) final switch(func.callableType) {
                        case NotCallable:
                            runtimeError("Can only call functions and classes.");
                            return InterpretResult.RUNTIME_ERROR;
                        case Function:
                            if(!callValue(func, argCount))
                                return InterpretResult.RUNTIME_ERROR;
                            frame = &frames[frameCount - 1];
                            break;
                        case Native:
                            auto result = callNative(func, argCount);
                            if(auto msg = result.getError()) {
                                runtimeError(msg);
                                return InterpretResult.RUNTIME_ERROR;
                            }
                            push(result);
                            break;
                    }
                    break;
                case RETURN:
                    auto result = pop();
                    --frameCount;
                    if(frameCount == 0) {
                        pop();
                        // Exit interpreter.
                        return InterpretResult.OK;
                    }

                    stackTop = frame.slots;
                    push(result);
                    frame = &frames[frameCount - 1];
                    break;
            }
        }
    }

    void resetStack() {
        stackTop = stack.ptr;
        frameCount = 0;
    }

    void push(Value value) {
        assert(stackTop < stack.ptr + stack.length);
        *stackTop++ = value;
    }

    Value pop() {
        assert(stackTop > stack.ptr);
        return *(--stackTop);
    }

    Value peek(int offset = 0) {
        assert(stackTop - offset - 1 >= stack.ptr);
        return *(stackTop - offset - 1);
    }

    void runtimeError(string msg) {
        errStream.writeln(msg, true);

        foreach_reverse(ref frame; frames[0 .. frameCount]) {
            auto fun = frame.fun;
            size_t instruction = frame.fun.chunk.code.length - frame.ip.length - 1;
            errStream.write(i"[line $(fun.chunk.lines[instruction])] in ", false);
            if(fun.name == null) {
                errStream.writeln("script");
            } else {
                errStream.writeln(i"$(fun.name)()");
            }
        }

        resetStack();
    }
    private bool callValue(Value func, int argCount) {
        auto fun = func.extractFunction();
        assert(fun);
        return call(fun, argCount);
    }

    private bool call(ObjFunction* fun, int argCount)
    {
        if(fun.arity != argCount) {
            import std.conv;
            runtimeError(i"Expected $(fun.arity) arguments but got $(argCount).".text);
            return false;
        }
        if(frameCount == frames.length)
        {
            runtimeError("Stack Overflow.");
            return false;
        }
        auto frame = &frames[vm.frameCount++];
        frame.fun = fun;
        frame.ip = fun.chunk.code;
        frame.slots = stackTop - argCount - 1;
        return true;
    }

    private Value callNative(Value fun, int argCount)
    {
        auto native = fun.extractNative();
        assert(native);
        // call with the given arguments
        auto argStart = stackTop - argCount;
        auto result = native.fun(argStart[0 .. argCount]);
        stackTop -= argCount + 1;
        return result;
    }

    private void defineNative(string name, ObjNative* value)
    {
        internString(name); // always called with a literal.
        globals[name] = Value(value);
    }
}

VM vm;

private double nativeClock() {
    import core.sys.posix.stdc.time : clock, CLOCKS_PER_SEC;
    return double(clock()) / CLOCKS_PER_SEC;
}

void initVM() {
    import core.stdc.math : sin;
    vm.resetStack();
    vm.defineNative("clock", getNativeFn!nativeClock);
    vm.defineNative("sin", getNativeFn!sin);
}

void freeVM() {
}

void push(Value value) {
    vm.push(value);
}

Value pop() {
    return vm.pop();
}

InterpretResult interpret(const(char)[] source) {
    auto fun = compile(source);

    if(!fun)
        return InterpretResult.COMPILE_ERROR;

    push(Value(fun));

    vm.call(fun, 0);

    return vm.run();
}
