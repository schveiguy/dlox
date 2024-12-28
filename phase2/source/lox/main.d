module lox.main;
import lox.chunk;
import lox.vm;

int main(string[] args)
{
    initVM();
    Chunk chunk;
    
    auto constant = chunk.addConstant(1.2);
    chunk.write(OpCode.CONSTANT, 123);
    chunk.write(constant, 123);
    
    constant = chunk.addConstant(3.4);
    chunk.write(OpCode.CONSTANT, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode.ADD, 123);
    
    constant = chunk.addConstant(5.6);
    chunk.write(OpCode.CONSTANT, 123);
    chunk.write(constant, 123);

    chunk.write(OpCode.DIVIDE, 123);
    chunk.write(OpCode.NEGATE, 123);

    chunk.write(OpCode.RETURN, 123);

    import lox.dbg;
    chunk.disassembleChunk("test chunk");
    interpret(&chunk);
    
    freeVM();
    return 0;
}
