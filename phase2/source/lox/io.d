module lox.io;

import iopipe.bufpipe;
import iopipe.refc;
import iopipe.valve;
import iopipe.textpipe;
import std.io;
import core.interpolation;

// because we are using iopipe and not Java or Phobos, I wanted to encapsulate
// all the io stuff in one module.

const(char)[] readText(string path) {
    auto f = File(path).refCounted.bufd.assumeText;
    f.ensureElems();
    return f.window;
}

private auto openOutputStream(File dev)
{
    // note, this returns a ref-counted struct, so there is no need to worry about copies.
    return bufd!char.push!(p => p.encodeText.outputPipe(dev.refCounted));
}

alias OutputStream = typeof(openOutputStream(File.init));

private auto openLineInputStream(File dev)
{
    return dev.refCounted.bufd.assumeText.byLine;
}

alias InputLines = typeof(openLineInputStream(File.init));

OutputStream outStream()
{
    import std.io.driver : stdout;
    static OutputStream _stream;
    //if(_stream.valve!OutputPipe.dev.allocated)
    if(_stream is _stream.init) //hate to do it this way, above would be better
    {
        _stream = openOutputStream(File(stdout));
    }
    return _stream;
}

OutputStream errStream()
{
    import std.io.driver : stderr;
    static OutputStream _stream;
    //if(_stream.valve!OutputPipe.dev.allocated)
    if(_stream is _stream.init) //hate to do it this way, above would be better
    {
        _stream = openOutputStream(File(stderr));
    }
    return _stream;
}

InputLines inStream()
{
    import std.io.driver : stdin;
    static InputLines _stream;
    //if(_stream.valve!BufferedInputSource.dev.allocated)
    if(_stream is _stream.init) //hate to do it this way, above would be better
    {
        _stream = openLineInputStream(File(stdin));
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

void writeln(OutputStream stream, bool doFlush = true)
{
    writeln(stream, "", doFlush);
}

void writeln(Args...)(OutputStream stream, InterpolationHeader head, Args args, InterpolationFooter foot)
{
    writeln(stream, head, args, foot, true);
}

void writeln(Args...)(OutputStream stream, InterpolationHeader, Args args, InterpolationFooter, bool doFlush)
{
    import std.conv;
    static foreach(i; 0 .. Args.length)
    {{
        static if(!is(Args[i] == InterpolatedExpression!s, string s))
        {
            stream.write(args[i].to!string, false);
        }
    }}
    stream.write("\n", doFlush);
}

void write(Args...)(OutputStream stream, InterpolationHeader head, Args args, InterpolationFooter foot)
{
    write(stream, head, args, foot, true);
}

void write(Args...)(OutputStream stream, InterpolationHeader, Args args, InterpolationFooter, bool doFlush)
{
    import std.conv;
    static foreach(i; 0 .. Args.length)
    {{
        static if(!is(Args[i] == InterpolatedExpression!s, string s))
        {
            stream.write(args[i].to!string, false);
        }
    }}
    if(doFlush)
        stream.flush();
}

const(char)[] nextLine(InputLines lines)
{
    lines.release(lines.window.length);
    lines.extend(0);
    // we need to dup the data because the source code might be reported to the
    // user, and if the buffer is reused, the data might be overwritten
    return lines.window.idup;
}

// format helper
auto formatted(T)(T value, string format)
{
    static struct FormattedValue
    {
        T item;
        string fmt;
        void toString(Out)(ref Out output)
        {
            import std.format : formattedWrite;
            formattedWrite(output, fmt, item);
        }

        string toString()
        {
            import std.array;
            Appender!string app;
            toString(app);
            return app.data;
        }
    }
    return FormattedValue(value, format);
}
