module lox.main;
import lox.chunk;

int main(string[] args)
{
    Chunk chunk;
    
    auto constant = chunk.addConstant(1.2);
    chunk.write(OpCode.OP_CONSTANT, 123);
    chunk.write(constant, 123);
    chunk.write(OpCode.RETURN, 123);

    import lox.dbg;
    chunk.disassembleChunk("test chunk");
    
    return 0;
}
