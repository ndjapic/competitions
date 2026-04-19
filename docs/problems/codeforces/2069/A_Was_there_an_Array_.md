# Problem: A_Was_there_an_Array_.pas

```pascal
program A_Was_there_an_Array_;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int8;
    b: array [1 .. nn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        for i := 2 to n-1 do read(b[i]); readln;
        b[1] := 0;
        b[n] := 0;

        i := 2;
        while (i <= n-1) and not (
            (b[i-1] = 1) and
            (b[i] = 0) and
            (b[i+1] = 1)
        ) do inc(i);

        if i <= n-1 then
            writeln('NO')
        else
            writeln('YES');

    end;
end.

```
