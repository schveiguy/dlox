module lox.dbg;

import lox.chunk;
import lox.value;
import lox.io;
import lox.obj;

void disassembleChunk(const ref Chunk chunk, string name)
{
    auto o = outStream;
    o.writeln(i"== $(name) ==");

    for(int offset = 0; offset < chunk.count;)
    {
        offset = disassembleInstruction(chunk, offset);
    }
}

int disassembleInstruction(const ref Chunk chunk, int offset)
{
    auto o = outStream;
    o.write(i`$(offset.formatted("%04d")) `, false);
    if (offset > 0 &&
            chunk.lines[offset] == chunk.lines[offset - 1]) {
        o.write("   | ", false);
    } else {
        o.write(i`$(chunk.lines[offset].formatted("%4d")) `, false);
    }

    ubyte instruction = chunk.code[offset];
    with(OpCode) switch(instruction) {
        case CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case NIL:
            return simpleInstruction("OP_NIL", offset);
        case TRUE:
            return simpleInstruction("OP_TRUE", offset);
        case FALSE:
            return simpleInstruction("OP_FALSE", offset);
        case DEFINE_GLOBAL:
            return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
        case GET_GLOBAL:
            return constantInstruction("OP_GET_GLOBAL", chunk, offset);
        case SET_GLOBAL:
            return constantInstruction("OP_SET_GLOBAL", chunk, offset);
        case GET_UPVALUE:
            return byteInstruction("OP_GET_UPVALUE", chunk, offset);
        case SET_UPVALUE:
            return byteInstruction("OP_SET_UPVALUE", chunk, offset);
        case GET_LOCAL:
            return byteInstruction("OP_GET_LOCAL", chunk, offset);
        case SET_LOCAL:
            return byteInstruction("OP_SET_LOCAL", chunk, offset);
        case JUMP_IF_FALSE:
            return jumpInstruction("OP_JUMP_IF_FALSE", chunk, 1, offset);
        case JUMP:
            return jumpInstruction("OP_JUMP", chunk, 1, offset);
        case LOOP:
            return jumpInstruction("OP_LOOP", chunk, -1, offset);
        case GET_PROPERTY:
            return constantInstruction("OP_GET_PROPERTY", chunk, offset);
        case SET_PROPERTY:
            return constantInstruction("OP_SET_PROPERTY", chunk, offset);
        case EQUAL:
            return simpleInstruction("OP_EQUAL", offset);
        case GREATER:
            return simpleInstruction("OP_GREATER", offset);
        case LESS:
            return simpleInstruction("OP_LESS", offset);
        case ADD:
            return simpleInstruction("OP_ADD", offset);
        case SUBTRACT:
            return simpleInstruction("OP_SUBTRACT", offset);
        case MULTIPLY:
            return simpleInstruction("OP_MULTIPLY", offset);
        case DIVIDE:
            return simpleInstruction("OP_DIVIDE", offset);
        case NEGATE:
            return simpleInstruction("OP_NEGATE", offset);
        case NOT:
            return simpleInstruction("OP_NOT", offset);
        case PRINT:
            return simpleInstruction("OP_PRINT", offset);
        case POP:
            return simpleInstruction("OP_POP", offset);
        case CALL:
            return byteInstruction("OP_CALL", chunk, offset);
        case CLOSURE:
            ++offset;
            ubyte constant = chunk.code[offset++];
            o.write(i`$("OP_CLOSURE".formatted("%-16s")) $(constant.formatted("%4d"))`);
            printValue(chunk.constants.values[constant]);
            o.writeln();

            auto fun = chunk.constants.values[constant].extractFunction;
            foreach(j; 0 .. fun.upvalueCount) {
                int isLocal = chunk.code[offset++];
                int index = chunk.code[offset++];
                o.writeln(i`$((offset - 2).formatted("%04d"))      |                     $(isLocal ? "local" : "upvalue") $(index)`);
            }
            return offset;
        case CLOSE_UPVALUE:
            return simpleInstruction("OP_CLOSE_UPVALUE", offset);
        case RETURN:
            return simpleInstruction("OP_RETURN", offset);
        case CLASS:
            return constantInstruction("OP_CLASS", chunk, offset);
        default:
            o.writeln(i"Unknown opcode $(instruction)");
            return offset + 1;
    }
}

private int simpleInstruction(string name, int offset)
{
    outStream.writeln(name);
    return offset + 1;
}

private int constantInstruction(string name, ref const Chunk chunk, int offset) {
    auto o = outStream;
    ubyte constant = chunk.code[offset + 1];
    o.write(i`$(name.formatted("%-16s")) '`, false);
    printValue(chunk.constants.values[constant]);
    o.writeln("'");
    return offset + 2;
}

private int byteInstruction(string name, ref const Chunk chunk, int offset) {
    auto o = outStream;
    ubyte slot = chunk.code[offset + 1];
    o.writeln(i`$(name.formatted("%-16s")) [$(slot.formatted("%4d"))]`);
    return offset + 2;
}

private int jumpInstruction(string name, ref const Chunk chunk, int multiplier, int offset) {
    auto o = outStream;
    int jumpOffset = chunk.code[offset + 1] | (chunk.code[offset + 2] << 8);
    jumpOffset *= multiplier;
    o.writeln(i`$(name.formatted("%-16s")) -> $(offset + 3 + jumpOffset)`);
    return offset + 3;
}
