module lox.lox;

import lox.scanner;
import lox.token;
import lox.io;


/// begin Lox "class"

bool hadError = false;

void run(const(char)[] script)
{
    auto scanner = new Scanner(script);
    Token[] tokens = scanner.scanTokens();

    auto output = outStream;
    // for now, just print the tokens
    foreach(token; tokens)
    {
        output.writeln(token.toString(), false);
    }
    output.flush();
}

private void runFile(string path) {
    auto text = readText(path);
    run(text);

    // Indicate an error in the exit code
    import core.stdc.stdlib : exit;
    if(hadError) exit(65);
}

void error(int line, string message) {
    report(line, "", message);
}

private void report(int line, string where, string message) {
    // TODO: fix this
    import std.conv : text;
    errStream.writeln(text("[line ", line, "] Error", where, ": ", message));
    hadError = true;
}

private void runPrompt() {
    import std.io.driver : stdin;
    auto lines = inStream();
    auto output = outStream();
    for(;;) {
        output.write("> ");
        auto l = lines.nextLine();
        if(l.length == 0)
            break;
        run(l);
        // processed the line, continue
        hadError = false;
    }
}

int main(string[] args) {
    if(args.length > 2) {
        outStream.writeln("Usage: dlox1 [script]");
        return 64;
    }
    else if(args.length == 2) {
        runFile(args[1]);
    } else {
        runPrompt();
    }
    return 0;
}
