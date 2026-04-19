# Problem: B_World_Meeting.pas

```pascal
program B_World_Meeting;
uses
    math;
const
    day = 24;
var
    n, i: int32;
    x: int8;
    w: int64;
    e: array [0 .. 3*day] of int64;

begin
    for x := 0 to 3*day do e[x] := 0;

    readln(n);

    for i := 1 to n do begin
        readln(w, x);
        inc(e[x+9], w);
        dec(e[x+18], w);
        inc(x, day);
        inc(e[x+9], w);
        dec(e[x+18], w);
    end;

    w := 0;
    for x := 0 to 3*day-1 do begin
        w := max(w, e[x]);
        inc(e[x+1], e[x]);
    end;

    writeln(w);
end.

```
