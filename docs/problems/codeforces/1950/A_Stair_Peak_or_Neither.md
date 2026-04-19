# Problem: A_Stair_Peak_or_Neither.pas

```pascal
program A_Stair_Peak_or_Neither;
var
    ntc, tci: int16;
    a, b, c: int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c);

        if a >= b then
            writeln('NONE')
        else if b < c then
            writeln('STAIR')
        else if b > c then
            writeln('PEAK')
        else
            writeln('NONE');

    end;
end.

```
