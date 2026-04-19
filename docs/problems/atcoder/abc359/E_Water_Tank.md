# Problem: E_Water_Tank.pas

```pascal
program E_Water_Tank;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    n, i, t: int32;
    h, s: array [0 .. nn] of int32;
    ans: array [0 .. nn] of int64;

begin
    readln(n);

    t := 0;
    s[0] := 0;
    h[0] := high(int32);
    ans[0] := 0;

    for i := 1 to n do begin

        read(h[i]);
        ans[i] := ans[s[t]] + int64(i-s[t]) * h[i];

        while (t > 0) and (h[i] > h[s[t]]) do begin
            inc(ans[i], int64(h[i] - h[s[t]]) * s[t]);
            dec(t);
        end;

        inc(t);
        s[t] := i;
        write(ans[i] + 1, ' ');

    end;
    readln;
    writeln;
end.

```
