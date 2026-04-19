# Problem: B_Increase_Decrease_Copy.pas

```pascal
program B_Increase_Decrease_Copy;
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, d, mn, mx, x: int32;
    s: int64;
    a, b: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n+1 do read(b[i]); readln;

        s := 1;
        d := high(int32);
        x := b[n+1];

        for i := 1 to n do begin

            mn := min(a[i], b[i]);
            mx := max(a[i], b[i]);

            inc(s, mx-mn);

            if x < mn then
                d := min(d, mn - x)
            else if x > mx then
                d := min(d, x - mx)
            else
                d := 0;

        end;

        writeln(s+d);

    end;
end.

```
