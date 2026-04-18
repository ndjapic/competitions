# Задатак: D_Line_Crossing.pas

```pascal
program D_Line_Crossing;
const
    nn = 1000 * 1000;
var
    n, m, i, a, b: int32;
    ans: int64;
    c: array [0 .. nn] of int32;

begin
    readln(n, m);

    for b := 0 to n-1 do c[b] := 0;

    for i := 1 to m do begin
        readln(a, b);
        inc(c[(a+b-2) mod n]);
    end;

    ans := 0;
    for b := 0 to n-1 do
        inc(ans, int64(m-c[b]) * c[b]);

    writeln(ans div 2);
end.

```
