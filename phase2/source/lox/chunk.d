module lox.chunk;

import lox.value;

enum OpCode : ubyte
{
    OP_CONSTANT,
    RETURN,
}

struct Chunk
{
    int count;
    ubyte[] _storage;
    int* _lines;
    ValueArray constants;
    
    inout(ubyte)[] code() inout {
        return _storage[0 .. count];
    }
    
    inout(int)[] lines() inout {
        return _lines[0 .. count];
    }

    void write(ubyte val, int line) {
        if(count == _storage.length)
        {
            // utilize appending amortized growth.
            _storage ~= val;
            auto l = _lines[0 .. count];
            l ~= line;
            _lines = l.ptr;
            ++count;
        }
        else
        {
            _storage[count] = val;
            _lines[count++] = line;
        }
    }
    
    ubyte addConstant(Value val) {
        constants.write(val);
        assert(constants.count <= ubyte.max + 1);
        return cast(ubyte)(constants.count - 1);
    }
}
