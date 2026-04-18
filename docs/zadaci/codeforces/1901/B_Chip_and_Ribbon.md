# Задатак: B_Chip_and_Ribbon.pas

```pascal
program B_Chip_and_Ribbon;
uses
    math;
const
    maxn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i: int32;
    ans: int64;
    c: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(c[i]);
        readln;
        c[n+1] := 0;

        ans := -1;
        for i := 1 to n do
            inc(ans, max(0, c[i] - c[i+1]));

        writeln(ans);

    end;
end.

```
