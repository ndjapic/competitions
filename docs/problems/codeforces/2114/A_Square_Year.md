# Problem: A_Square_Year.pas

```pascal
program A_Square_Year;
var
    ntc, tci: int16;
    y, a: int32;
    i: int8;
    ch: char;

function isqrt(a: int32): int32;
var
    x: int32;
begin
    x := a;
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        y := 0;
        for i := 1 to 4 do begin
            read(ch);
            y := 10 * y + ord(ch) - ord('0');
        end;
        readln;

        a := isqrt(y);
        if sqr(a) = y then
            writeln(a, ' 0')
        else
            writeln(-1);

    end;
end.

```
