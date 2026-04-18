# Задатак: B_Large_Addition.pas

```pascal
program B_Large_Addition;
const
    ee = 18;
var
    ntc, tci: int16;
    x: int64;
    e, i: int8;
    a: array [0 .. ee] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x);

        e := 0;
        while x > 0 do begin
            a[e] := x mod 10;
            x := x div 10;
            inc(e);
        end;
        dec(e);

        if (a[0] = 9) or (a[e] <> 1) then
            writeln('NO')
        else begin

            i := 1;
            while (i < e) and (a[i] > 0) do inc(i);

            if i < e then
                writeln('NO')
            else
                writeln('YES');

        end;

    end;
end.

```
