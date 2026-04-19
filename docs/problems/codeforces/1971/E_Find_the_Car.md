# Problem: E_Find_the_Car.pas

```pascal
program E_Find_the_Car;
{$mode delphi}
uses
    math;
const
    kq = 100 * 1000 + 1;
var
    ntc, tci: int16;
    n, k, q, d, i, j, l, r, m: int32;
    a, b: array [0 .. kq] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, q);

        a[0] := 0;
        b[0] := 0;
        for i := 1 to k do read(a[i]); readln;
        for i := 1 to k do read(b[i]); readln;
        a[k+1] := a[k] + 1;
        b[k+1] := b[k] + 1;

        for j := 1 to q do begin

            readln(d);
            l := 0;
            r := k+1;

            while r-l > 1 do begin
                m := (l+r) div 2;
                if d < a[m] then
                    r := m
                else
                    l := m;
            end;

            i := l;
            l := b[l];
            r := b[r];

            while r-l > 1 do begin
                m := (l+r) div 2;
                {if d < a[i] + (a[i+1]-a[i]) / (b[i+1]-b[i]) * (m-b[i]) then}
                if int64(d-a[i]) * (b[i+1]-b[i]) < int64(a[i+1]-a[i]) * (m-b[i]) then
                    r := m
                else
                    l := m;
            end;

            write(l);
            if j < q then write(' ');

        end;
        writeln;

    end;
end.

```
