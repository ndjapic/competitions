# Задатак: A_Gellyfish_and_Tricolor_Pansy.pas

```pascal
program A_Gellyfish_and_Tricolor_Pansy;
uses
    math;
var
    ntc, tci, a, b, c, d: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, c, d);

        if min(b, d) > min(a, c) then
            writeln('Flower')
        else
            writeln('Gellyfish');

    end;
end.

```
