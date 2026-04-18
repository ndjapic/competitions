# Задатак: C_Standing_On_The_Shoulders.pas

```pascal
program C_Standing_On_The_Shoulders;
uses
    math;
const
    nn = 200 * 1000;
var
    n, i, a, b, mx: int32;
    s: int64;

begin
    readln(n);
    mx := 0;
    s := 0;

    for i := 1 to n do begin
        readln(a, b);
        inc(s, a);
        mx := max(mx, b-a);
    end;

    writeln(s + mx);
end.

```
