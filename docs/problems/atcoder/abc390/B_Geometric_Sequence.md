# Problem: B_Geometric_Sequence.pas

```pascal
program B_Geometric_Sequence;
const
    nn = 100;
var
    n, i: int8;
    a: array [1 .. nn] of int64;

begin
    readln(n);
    for i := 1 to n do read(a[i]);
    readln;

    i := 2;
    while (i < n) and (a[i-1] * a[i+1] = sqr(a[i])) do inc(i);

    if i = n then
        writeln('Yes')
    else
        writeln('No');
end.

```
