# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
    n, u, v, a, b, c: int8;
    i, m: int16;
    ans: int32;
    e: array [1 .. 100, 1 .. 100] of int8;

begin
    readln(n, m);

    for u := 1 to n do
        for v := u+1 to n do
            e[u, v] := 0;

    for i := 1 to m do begin
        readln(u, v);
        e[u, v] := 1;
    end;

    ans := 0;
    for a := 1 to n do
        for b := a+1 to n do
            for c := b+1 to n do
                inc(ans, e[a, b] * e[a, c] * e[b, c]);

    writeln(ans);
end.

```
