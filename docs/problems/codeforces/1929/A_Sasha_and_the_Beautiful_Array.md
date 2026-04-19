# Problem: A_Sasha_and_the_Beautiful_Array.pas

```pascal
program A_Sasha_and_the_Beautiful_Array;
uses
    math;
const
    inf = 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i: int8;
    ai, mn, mx: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        mx := 1;
        mn := inf;

        for i := 1 to n do begin
            read(ai);
            mx := max(mx, ai);
            mn := min(mn, ai);
        end;
        readln;

        writeln(mx - mn);

    end;
end.

```
