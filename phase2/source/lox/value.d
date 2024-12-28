module lox.value;

alias Value = double;

struct ValueArray {
    Value[] _storage;
    int count;
    inout(Value[]) values() inout {
        return _storage[0 .. count];
    }
    
    void write(Value val) {
        if(count == _storage.length)
        {
            // utilize appending amortized growth.
            _storage ~= val;
            ++count;
        }
        else
            _storage[count++] = val;
    }
}

void printValue(Value value) {
    import lox.io;
    outStream.write(i"$(value)");
}
