# Задатак: C_Move_It.pas

```pascal
program C_Move_It;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 100 * 1000;
var
    n, i, j, w: int32;
    ans: int32;
    a, s, m: array [1 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        s[i] := 0;
        m[i] := 0;
    end;
    readln;

    for i := 1 to n do begin

        j := a[i];
        read(w);
        inc(s[j], w);
        m[j] := max(m[j], w);

    end;
    readln;

    ans := 0;
    for i := 1 to n do inc(ans, s[i]-m[i]);
    writeln(ans);
end.

```
