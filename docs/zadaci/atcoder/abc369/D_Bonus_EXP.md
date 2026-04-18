# Задатак: D_Bonus_EXP.pas

```pascal
program D_Bonus_EXP;
uses
    math;
const
    nn = 200 * 1000;
var
    n, i: int32;
    a: array [1 .. nn] of int32;
    dp0, dp1: array [1 .. nn] of int64;

begin
    readln(n);

    read(a[1]);
    dp0[1] := 0;
    dp1[1] := a[1];

    for i := 2 to n do begin
        read(a[i]);
        dp0[i] := max(dp0[i-1], dp1[i-1] + a[i]*2);
        dp1[i] := max(dp1[i-1], dp0[i-1] + a[i]);
    end;
    readln;

    writeln(max(dp0[n], dp1[n]));
end.

```
