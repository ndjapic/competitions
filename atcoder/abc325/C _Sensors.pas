program B_World_Meeting;
uses
    math;
const
    maxn = 1000;
var
    n, i, w: int32;
    x: int8;
    e: array [0 .. 44] of int32;

begin
    for x := 0 to 44 do e[x] := 0;

    readln(n);

    for i := 1 to n do begin
        readln(w, x);
        inc(e[x+9], w);
        dec(e[x+18], w);
    end;

    w := 0;
    for x := 0 to 43 do begin
        w := max(w, e[x]);
        inc(e[x+1], e[x]);
    end;

    writeln(w);
end.
