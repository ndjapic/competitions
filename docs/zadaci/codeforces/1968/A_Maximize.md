# Задатак: A_Maximize.pas

```pascal
program A_Maximize;
uses
    math;
var
    ntc, tci, x, y, i: int16;

function gcd(x, y: int16): int16;
begin
    if y = 0 then
        gcd := x
    else
        gcd := gcd(y, x mod y);
end;

function f(x, y: int16): int16;
begin
    f := gcd(x, y) + y;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x);

        y := 1;
        for i := 1 to x-1 do
            if f(x, i) > f(x, y) then y := i;

        writeln(y);

    end;
end.

```
