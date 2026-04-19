# Problem: D_Beauty_of_the_mountains.pas

```pascal
program D_Beauty_of_the_mountains;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 500;
var
    ntc, tci: int16;
    n, m, i, j, k, p: int16;
    g: int32;
    t: int64;
    s: string;
    a: array [1 .. nn, 1 .. nn] of int32;
    d: array [0 .. nn, 0 .. nn] of int32;

function gcd(x, y: int32): int32;
begin
    if x = 0 then
        result := y
    else
        result := gcd(y mod x, x);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

        for i := 1 to n do begin

            readln(s);
            s := s + ' ';

            p := 1;
            for j := 1 to m do begin
                a[i, j] := 0;
                while s[p] <> ' ' do begin
                    a[i, j] := a[i, j] * 10 + ord(s[p]) - ord('0');
                    inc(p);
                end;
                inc(p);
            end;

        end;

        for j := 0 to m do d[0, j] := 0;

        t := 0;
        for i := 1 to n do begin
            readln(s);
            d[i, 0] := 0;

            for j := 1 to m do begin

                d[i, j] := (ord(s[j]) - ord('0')) * 2 - 1;
                a[i, j] := a[i, j] * d[i, j];
                inc(t, a[i, j]);

                inc(d[i, j], d[i, j-1]);
                inc(d[i, j], d[i-1, j]);
                dec(d[i, j], d[i-1, j-1]);

            end;
        end;

        g := 0;
        for i := n downto k do
            for j := m downto k do begin
                dec(d[i, j], d[i, j-k]);
                dec(d[i, j], d[i-k, j]);
                inc(d[i, j], d[i-k, j-k]);
                g := gcd(g, abs(d[i, j]))
            end;

        if (t = 0) or (g > 0) and (t mod g = 0) then
            writeln('YES')
        else
            writeln('NO');

    end;
end.

```
