# Problem: A_Square.pas

```pascal
program A_Square;
uses
    math;
var
    ntc, tci: int8;
    i: int8;
    x, y, mn, mx, s: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        mn := 1000;
        mx := -1000;
        for i := 1 to 4 do begin
            readln(x, y);
            mn := min(mn, x);
            mx := max(mx, x);
        end;

        s := mx - mn;
        writeln(s*s);

    end;
end.

```
