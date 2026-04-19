# Problem: A_Card_Exchange.pas

```pascal
program A_Card_Exchange;
uses
    math;
const
    sz = 100;
var
    ntc, tci: int16;
    n, k, i, x, mx, ans: int8;
    f: array [1 .. sz] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for x := 1 to sz do f[x] := 0;
        mx := 0;

        for i := 1 to n do begin
            read(x);
            inc(f[x]);
            mx := max(mx, f[x]);
        end;
        readln;

        if mx >= k then
            ans := k-1
        else
            ans := n;

        writeln(ans);

    end;
end.

```
