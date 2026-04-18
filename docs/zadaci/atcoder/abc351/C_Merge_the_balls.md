# Задатак: C_Merge_the_balls.pas

```pascal
program C_Merge_the_balls;
const
    sz = 200 * 1000;
var
    n, i, t: int32;
    a: array [1 .. sz] of int64;

begin
    readln(n);
    t := 0;

    for i := 1 to n do begin
        inc(t);
        read(a[t]);
        while (t > 1) and (a[t-1] = a[t]) do begin
            dec(t);
            inc(a[t]);
        end;
    end;
    readln;

    writeln(t);
end.

```
