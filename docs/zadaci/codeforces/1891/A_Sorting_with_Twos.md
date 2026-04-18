# Задатак: A_Sorting_with_Twos.pas

```pascal
program A_Sorting_with_Twos;
const
    maxn = 20;
var
    ntc, tci: int16;
    n, i: int32;
    a: array [1 .. maxn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);
        for i := 1 to n do read(a[i]); readln;

        i := 1;
        while (i < n) and ((i and (i-1) = 0) or (a[i] <= a[i+1])) do inc(i);

        if i = n then
            writeln('YES')
        else
            writeln('NO');
    end;
end.

```
