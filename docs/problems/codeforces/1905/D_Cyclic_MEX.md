# Problem: D_Cyclic_MEX.pas

```pascal
program D_Cyclic_MEX;
uses
    math;
const
    maxn = 1000 * 1000;
var
    ntc, tci: int32;
    n, i: int32;
    x: int64;
    p, q, mx: array [0 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 0 to n-1 do begin
            read(p[i]);
            q[p[i]] := i;
        end;
        readln;

        mx[0] := q[0];
        x := 0;

        for i := 1 to n-1 do begin
            mx[i] := max(mx[i-1], (q[i] - q[0] + n) mod n);
            inc(x, int64(mx[i] - mx[i-1]) * i);
        end;

        writeln(x);

    end;
end.

```
