module lox.compiler;

import lox.scanner;
import lox.io;

void compile(const(char)[] source)
{
    initScanner(source);
    int line = -1;
    while(true) {
        Token token = scanToken();
        if(token.line != line) {
            outStream.write(i`$(token.line.formatted("%4d ")) `, false);
            line = token.line;
        } else {
            outStream.write("   | ", false);
        }
        outStream.writeln(i`$(token.type.formatted("%2d")) '$(token.lexeme)'`);

        if(token.type == TokenType.EOF) break;
    }
}
