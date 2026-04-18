program A_Takahashi_san;
const
    maxn = 21;
var
    n, i: int8;
    s: array [1 .. maxn] of char;

begin
    n := 0;
    repeat
        inc(n);
        read(s[n]);
    until eoln;
    readln;

    i := 1;
    while s[i] <> ' ' do begin
        write(s[i]);
        inc(i);
    end;

    write(' ');
    write('s');
    write('a');
    writeln('n');
end.
