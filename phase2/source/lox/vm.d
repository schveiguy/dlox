module lox.vm;

import lox.chunk;
import lox.io;
import lox.value;
import lox.compiler;

enum InterpretResult {
    OK,
    COMPILE_ERROR,
    RUNTIME_ERROR,
}

enum STACK_MAX = 256;

debug = TRACE_EXECUTION;

struct VM {
    Chunk *chunk;
    ubyte[] ip;
    Value[STACK_MAX] stack;
    Value* stackTop;
    InterpretResult run() {
        ubyte READ_BYTE() {
            scope(success) ip = ip[1 .. $];
            return ip[0];
        }

        Value READ_CONSTANT() => chunk.constants.values[READ_BYTE()];

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
                disassembleInstruction(*chunk, cast(int)(chunk.code.length - ip.length));
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
                case ADD:
                    if(!BINARY_OP!"+"()) return InterpretResult.RUNTIME_ERROR;
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
                    auto result = val.not();
                    // note, this never is an error. All values can be NOT-ed
                    assert(result.getError() is null);
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
                case RETURN:
                    printValue(pop());
                    outStream.writeln();
                    return InterpretResult.OK;
            }
        }
    }

    void resetStack() {
        stackTop = stack.ptr;
    }

    void push(Value value) {
        assert(stackTop < stack.ptr + stack.length);
        *stackTop++ = value;
    }

    Value pop() {
        assert(stackTop > stack.ptr);
        return *(--stackTop);
    }

    void runtimeError(string msg) {
        errStream.writeln(msg, false);
        size_t instruction = ip.ptr - chunk.code.ptr - 1;
        int line = chunk.lines[instruction];
        errStream.writeln(i`[line $(line)] in script`);
    }
}

VM vm;

void initVM() {
    vm.resetStack();
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
    Chunk chunk;

    if(!compile(source, &chunk))
        return InterpretResult.COMPILE_ERROR;

    vm.chunk = &chunk;
    vm.ip = vm.chunk.code;

    InterpretResult result = vm.run();
    return result;
}
