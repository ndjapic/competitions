# Задатак: A_Diagonals.pas

```pascal
program A_Diagonals;
uses
    math;
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, k, ans: int16;
    a: array [0 .. 2*nn] of int16;

begin
    a[0] := 0;
    for i := 1 to 2*nn do a[i] := a[i-1] + (i+1) div 2;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        k := n*n - k;

        i := 1;
        while a[i] <= k do inc(i);
        ans := 2*n-i;
        writeln(ans);

    end;
end.

```
