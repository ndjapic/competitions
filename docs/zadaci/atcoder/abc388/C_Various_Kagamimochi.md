# Задатак: C_Various_Kagamimochi.pas

```pascal
program C_Various_Kagamimochi;
uses
    math;
const
    nn = 500 * 1000;

var
    n, l, r: int32;
    ans: int64;
    a: array [1 .. nn] of int32;

begin
    readln(n);
    read(a[1]);

    ans := 0;
    l := 1;
    for r := 2 to n do begin
        read(a[r]);
        while a[l] <= a[r] div 2 do inc(l);
        inc(ans, l-1);
    end;
    readln;

    writeln(ans);
end.

```
