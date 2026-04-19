# Problem: C_Kevin_and_Puzzle.pas

```pascal
program C_Kevin_and_Puzzle;
{$mode objfpc}{$h+}
uses
    math;
const
    nn = 200 * 1000;
    prime = 998244353;

var
    ntc, tci: int16;
    n, i: int32;
    a, h, l: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        if a[1] = 0 then begin
            h[1] := 1;
            l[1] := 1;
        end else begin
            h[1] := 0;
            l[1] := 1;
        end;

        for i := 2 to n do
            if a[i] < i then begin
                h[i] := h[i-1] + l[i-1];
                l[i] := h[i-1];
            end else begin
                h[i] := 0;
                l[i] := h[i-1];
            end;

        writeln(h[n] + l[n]);

    end;
end.

```
