# Problem: B_Yet_Another_Coin_Problem.pas

```pascal
program B_Yet_Another_Coin_Problem;
uses
    math;
const
    inf = 15 * 100 * 1000;
var
    ntc, tci: int16;
    n: int32;
    d: array [0 .. inf] of int32;

function f(n: int32): int32;
begin
    f := n div inf * (inf div 15) + d[n mod inf];
end;

begin
    d[0] := 0;
    for n := 1 to inf do begin
        d[n] := 1 + d[n-1];
        if n >= 3 then d[n] := min(d[n], 1 + d[n-3]);
        if n >= 6 then d[n] := min(d[n], 1 + d[n-6]);
        if n >= 10 then d[n] := min(d[n], 1 + d[n-10]);
        if n >= 15 then d[n] := min(d[n], 1 + d[n-15]);
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        writeln(f(n));

    end;
end.

```
