module lox.dbg;

import lox.chunk;
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

    ubyte instruction = chunk.code[offset];
    switch(instruction) {
        case OpCode.RETURN:
            return simpleInstruction("OP_RETURN", offset);
        default:
            o.writeln("Unknown opcode $(instruction)");
    }
}

private int simpleInstruction(string name, int offset)
{
    outStream.writeln(name);
    return offset + 1;
}
