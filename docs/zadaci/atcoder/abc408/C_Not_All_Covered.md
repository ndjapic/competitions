# Задатак: C_Not_All_Covered.pas

```pascal
program C_Not_All_Covered;
uses
    math;
const
    nn = 1000 * 1000;
    mm = 200 * 1000;
var
    n, m, i, x, l, r, mn: int32;
    c: array [0 .. nn] of int32;

begin
    readln(n, m);

    for x := 0 to n do c[x] := 0;

    for i := 1 to m do begin
        readln(l, r);
        inc(c[l-1]);
        dec(c[r]);
    end;

    mn := m;
    for x := 0 to n-1 do begin
        mn := min(mn, c[x]);
        inc(c[x+1], c[x]);
    end;

    writeln(mn);
end.

```
