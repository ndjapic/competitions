# Задатак: B_Farthest_Point.pas

```pascal
program B_Farthest_Point;
const
    maxn = 100;
var
    n, i, j, j0: int8;
    x, y: array [1 .. maxn] of int32;

function d2(i, j: int8): int32;
begin
    d2 := sqr(x[i]-x[j]) + sqr(y[i]-y[j]);
end;

begin
    readln(n);

    for i := 1 to n do readln(x[i], y[i]);

    for i := 1 to n do begin
        j0 := 1;
        for j := 1 to n do
            if d2(i, j0) < d2(i, j) then j0 := j;
        writeln(j0);
    end;
end.

```
