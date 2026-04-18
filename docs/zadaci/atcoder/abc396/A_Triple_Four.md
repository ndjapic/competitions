# Задатак: A_Triple_Four.pas

```pascal
program A_Triple_Four;
const
    nn = 200 * 1000;

var
    n, i: int8;
    a: array [1 .. nn] of int8;

begin
    readln(n);

    for i := 1 to n do read(a[i]); readln;

    i := 1;
    while (i+2 <= n) and not ((a[i] = a[i+1]) and (a[i] = a[i+2])) do
        inc(i);

    if i+2 <= n then
        writeln('Yes')
    else
        writeln('No');
end.

```
