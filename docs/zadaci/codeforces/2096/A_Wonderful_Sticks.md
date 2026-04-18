# Задатак: A_Wonderful_Sticks.pas

```pascal
program A_Wonderful_Sticks;
{$MODE DELPHI}
const
    nn = 100;
var
    ntc, tci: int16;
    n, i, j: int8;
    s: string;
    a: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);
        a[1] := 1;

        for i := 2 to n do
            case s[i-1] of
                '>': a[i] := i;
                '<': begin
                    for j := 1 to i-1 do inc(a[j]);
                    a[i] := 1;
                end;
            end;

        for i := 1 to n-1 do write(a[i], ' ');
        writeln(a[n]);

    end;
end.

```
