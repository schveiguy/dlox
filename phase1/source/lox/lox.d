module lox.lox;

import lox.scanner;
import lox.token;

import iopipe.bufpipe;
import iopipe.refc;
import iopipe.valve;
import iopipe.textpipe;
import std.io;

// helper utils
char[] readText(string path) {
    auto f = File(path).refCounted.bufd.assumeText;
    f.ensureElems();
    return f.window;
}

auto openStream(File dev)
{
    import std.algorithm : move;
    // note, this returns a ref-counted struct, so there is no need to worry about copies.
    return bufd!char.push!(p => p.arrayCastPipe!ubyte.outputPipe(move(dev).refCounted));
}

alias OutputStream = typeof(openStream(File.init));

OutputStream outStream()
{
    import std.io.driver : stdout;
    static OutputStream _stream;
    //if(_stream.valve!OutputPipe.dev.allocated)
    if(_stream is OutputStream.init) //hate to do it this way, above would be better
    {
        _stream = openStream(File(stdout));
    }
    return _stream;
}

OutputStream errStream()
{
    import std.io.driver : stderr;
    static OutputStream _stream;
    //if(_stream.valve!OutputPipe.dev.allocated)
    if(_stream is OutputStream.init) //hate to do it this way, above would be better
    {
        _stream = openStream(File(stderr));
    }
    return _stream;
}

void write(OutputStream stream, const(char)[] data, bool doFlush = true)
{
    stream.ensureElems(data.length);
    stream.window[0 .. data.length] = data;
    stream.release(data.length);
    if(doFlush)
        stream.flush();
}

void writeln(OutputStream stream, const(char)[] data, bool doFlush = true)
{
    stream.ensureElems(data.length + 1);
    stream.window[0 .. data.length] = data;
    stream.window[data.length] = '\n';
    stream.release(data.length + 1);
    if(doFlush)
        stream.flush();
}

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
    auto lines = File(stdin).refCounted.bufd.assumeText.byLine;
    auto output = outStream();
    for(;;) {
        output.write("> ");
        if(lines.extend(0) == 0)
            // no more data
            break;
        run(lines.window);
        // processed the line, continue
        lines.release(lines.window.length);
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
