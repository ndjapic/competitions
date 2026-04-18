# Задатак: A_Strange_Splitting.pas

```pascal
program A_Strange_Splitting;
{$H+}
const
    nn = 50;
var
    ntc, tci: int8;
    n, i: int8;
    s: string;
    a: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        if a[1] = a[n] then
            writeln('NO')
        else begin

            writeln('YES');

            setlength(s, n);
            for i := 1 to n do s[i] := 'R';
            s[2] := 'B';
            writeln(s);

        end;

    end;
end.

```
