module lox.main;
import lox.chunk;
import lox.vm;
import lox.io;

int main(string[] args)
{
    initVM();
    scope(exit) freeVM();

    if(args.length == 1)
        repl();
    else if(args.length == 2)
        return runFile(args[1]);
    else
    {
        errStream.writeln("Usage: dlox2 [path]");
        return 64;
    }
    return 0;
}

private void repl() {
    while(true) {
        outStream.write("> ");
        auto line = inStream.nextLine;
        if(line.length == 0)
            break;
        interpret(line);
    }
}

private int runFile(string path) {
    auto source = readText(path);
    auto result = interpret(source);

    if(result == InterpretResult.COMPILE_ERROR) return 65;
    if(result == InterpretResult.RUNTIME_ERROR) return 70;
    return 0;
}
