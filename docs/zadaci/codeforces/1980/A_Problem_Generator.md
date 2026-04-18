# Задатак: A_Problem_Generator.pas

```pascal
program A_Problem_Generator;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    n, m, i, j: int8;
    ans: int16;
    a: string;
    c: array [0 .. 6] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        readln(a);

        for j := 0 to 6 do c[j] := 0;

        for i := 1 to n do begin
            j := ord(a[i]) - ord('A');
            inc(c[j]);
        end;

        ans := 0;
        for j := 0 to 6 do
            inc(ans, max(0, m-c[j]));
        writeln(ans);

    end;
end.

```
