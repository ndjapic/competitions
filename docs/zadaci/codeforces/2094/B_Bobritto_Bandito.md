# Задатак: B_Bobritto_Bandito.pas

```pascal
program B_Bobritto_Bandito;
uses
    math;
var
    ntc, tci: int8;
    n, m, l, r: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, l, r);

        inc(l, n-m);
        if l > 0 then begin
            dec(r, l);
            l := 0;
        end;

        writeln(l, ' ', r);

    end;
end.

```
