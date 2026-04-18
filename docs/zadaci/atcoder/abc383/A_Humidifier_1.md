# Задатак: A_Humidifier_1.pas

```pascal
program A_Humidifier_1;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math;
var
    n, i, t0, t1, v: int8;
    ans: int16;

begin
    readln(n);
    ans := 0;
    t0 := 0;

    for i := 1 to n do begin
        readln(t1, v);
        ans := max(0, ans - t1+t0);
        inc(ans, v);
        t0 := t1;
    end;

    writeln(ans);
end.

```
