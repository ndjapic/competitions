# Задатак: B_Nutrients.pas

```pascal
program B_Nutrients;
const
    nn = 100;
var
    n, m, i, j: int8;
    x: int32;
    a: array [1 .. nn] of int32;

begin
    readln(n, m);
    for j := 1 to m do read(a[j]); readln;

    for i := 1 to n do begin
        for j := 1 to m do begin
            read(x);
            dec(a[j], x);
        end;
        readln;
    end;

    j := 1;
    while (j <= m) and (a[j] < 0) do inc(j);

    if j <= m then
        writeln('No')
    else
        writeln('Yes');
end.

```
