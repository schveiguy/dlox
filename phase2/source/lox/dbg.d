module lox.dbg;

import lox.chunk;
import lox.value;
import lox.io;

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
    switch(instruction) {
        case OpCode.OP_CONSTANT:
            return constantInstruction("OP_CONSTANT", chunk, offset);
        case OpCode.RETURN:
            return simpleInstruction("OP_RETURN", offset);
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
    o.write(i`$(name.formatted("%-16s")) $(offset.formatted("%4d")) '`, false);
    printValue(chunk.constants.values[constant]);
    o.writeln("'");
    return offset + 2;
}
