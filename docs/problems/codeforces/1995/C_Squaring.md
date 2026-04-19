# Problem: C_Squaring.pas

```pascal
program C_Squaring;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    ans: int64;
    a, c: array [1 .. nn] of int64;

function isqrt(a: int64): int64;
var
    x: int64;
begin
    x := min(a, high(int32));
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        ans := 0;
        c[1] := 0;
        i := 2;

        while (i <= n) and (ans > -1) do begin
            if (a[i-1] > 1) and (a[i] = 1) then
                ans := -1
            else begin
                c[i] := c[i-1];

                while sqr(a[i-1]) <= a[i] do begin
                    a[i] := isqrt(a[i]);
                    dec(c[i]);
                end;

                while a[i-1] > a[i] do begin
                    a[i-1] := isqrt(a[i-1]);
                    inc(c[i]);
                end;

                inc(ans, c[i]);
            end;
            inc(i);
        end;

        writeln(ans);

    end;
end.

```
