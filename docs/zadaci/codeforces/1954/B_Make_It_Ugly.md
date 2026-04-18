# Задатак: B_Make_It_Ugly.pas

```pascal
program B_Make_It_Ugly;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 300 * 1000;
var
    ntc, tci: int16;
    n, i, ans, c: int32;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;

        c := 0;
        ans := n;

        for i := 1 to n+1 do
            if (i < n+1) and (a[i] = a[1]) then begin
                inc(c)
            end else if c > 0 then begin
                ans := min(ans, c);
                c := 0;
            end;

        if ans = n then ans := -1;
        writeln(ans);

    end;
end.

```
