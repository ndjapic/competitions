# Задатак: C_Choose_Elements.pas

```pascal
program C_Choose_Elements;
const
    maxn = 200 * 1000;
var
    n, k, i: int32;
    a, b: array [1 .. maxn] of int32;
    dpa, dpb: array [1 .. maxn] of boolean;

begin
    readln(n, k);

    for i := 1 to n do read(a[i]); readln;
    for i := 1 to n do read(b[i]); readln;

    dpa[1] := true;
    dpb[1] := true;

    for i := 2 to n do begin
        dpa[i] := (dpa[i-1] and (abs(a[i] - a[i-1]) <= k)) or (dpb[i-1] and (abs(a[i] - b[i-1]) <= k));
        dpb[i] := (dpa[i-1] and (abs(b[i] - a[i-1]) <= k)) or (dpb[i-1] and (abs(b[i] - b[i-1]) <= k));
    end;

    if dpa[n] or dpb[n] then
        writeln('Yes')
    else
        writeln('No');
end.

```
