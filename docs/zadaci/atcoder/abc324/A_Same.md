# Задатак: A_Same.pas

```pascal
program A_Same;
const
    maxn = 100;
var
    n, i: int8;
    a: array [1 .. maxn] of int8;

begin
    readln(n);
    for i := 1 to n do read(a[i]);

    i := 1;
    while (i < n) and (a[i] = a[i+1]) do inc(i);

    if i < n then
        writeln('No')
    else
        writeln('Yes');
end.

```
