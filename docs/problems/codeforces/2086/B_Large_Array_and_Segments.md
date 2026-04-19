# Problem: B_Large_Array_and_Segments.pas

```pascal
program B_Large_Array_and_Segments;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, k, i: int32;
    x, l: int64;
    a: array [1 .. nn] of int32;
    s: array [0 .. nn] of int64;

function f(l: int64): int64;
begin
    dec(l);
    f := s[n] * (k - l div n) - s[l mod n];
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, x);

        s[0] := 0;
        for i := 1 to n do begin
            read(a[i]);
            s[i] := s[i-1] + a[i];
        end;
        readln;

        l := int64(n) * k + 1;
        while (l > 1) and (f(l) < x) do dec(l, n);
        while (l <= int64(n) * k) and (f(l) >= x) do inc(l);

        writeln(l-1);

    end;
end.

```
