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

        void BINARY_OP(string op)() {
            auto b = pop();
            auto a = pop();
            push(mixin("a " ~ op ~ " b"));
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
            final switch(instruction = READ_BYTE()) {
                case OpCode.CONSTANT:
                    Value constant = READ_CONSTANT();
                    push(constant);
                    break;
                case OpCode.ADD:
                    BINARY_OP!"+"();
                    break;
                case OpCode.SUBTRACT:
                    BINARY_OP!"-"();
                    break;
                case OpCode.MULTIPLY:
                    BINARY_OP!"*"();
                    break;
                case OpCode.DIVIDE:
                    BINARY_OP!"/"();
                    break;
                case OpCode.NEGATE:
                    push(-pop());
                    break;
                case OpCode.RETURN:
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
