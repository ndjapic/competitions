# Problem: A_False_Alarm.pas

```pascal
program A_False_Alarm;
uses
    math;
const
    nn = 10;
var
    ntc, tci, n, x, i, l, r: int16;
    a: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        for i := 1 to n do read(a[i]); readln;

        l := 1;
        r := n;
        while a[l] = 0 do inc(l);
        while a[r] = 0 do dec(r);

        if r-l < x then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
