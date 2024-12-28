module lox.main;
import lox.chunk;

int main(string[] args)
{
    Chunk chunk;
    chunk.write(OpCode.RETURN);

    chunk.disassembleChunk("test chunk");
    
    return 0;
}
