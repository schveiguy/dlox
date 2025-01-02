module lox.obj;

import lox.chunk;

struct ObjFunction {
    int arity;
    Chunk chunk;
    string name;
}
