# Задатак: E_Queue_Sort.pas

```pascal
program E_Queue_Sort;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, m: int32;
    a: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        m := 1;

        for i := 1 to n do begin
            read(a[i]);
            if a[i] < a[m] then m := i;
        end;
        readln;

        i := m;
        while (i < n) and (a[i] <= a[i+1]) do inc(i);

        if i < n then m := 0;
        writeln(m-1);

    end;
end.

```
