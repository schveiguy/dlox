module lox.chunk;

enum OpCode : ubyte
{
    RETURN,
}

struct Chunk
{
    size_t count;
    ubyte[] _storage;
    inout(ubyte)[] code() inout {
        return _storage[0 .. count];
    }

    void write(ubyte val) {
        if(count == _storage.length)
        {
            _storage.length += 1;
            _storage = _storage.ptr[0 .. _storage.capacity];
        }
        _storage[count++] = val;
    }
}
