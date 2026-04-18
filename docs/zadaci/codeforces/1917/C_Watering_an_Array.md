# Задатак: C_Watering_an_Array.pas

```pascal
program C_Watering_an_Array;
uses
    math;
const
    maxn = 2000;
    maxk = 100 * 1000;
var
    ntc, tci: int16;
    n, k, d, i, j, score, ans: int32;
    a: array [1 .. maxn] of int32;
    v: array [1 .. maxk] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, d);

        for i := 1 to n do read(a[i]);
        readln;

        for j := 1 to k do read(v[j]);
        readln;

        ans := 0;
        for j := 1 to min(2*n+1, d) do begin

            score := (d-j) div 2;
            for i := 1 to n do
                if a[i] = i then inc(score);
            ans := max(ans, score);

            for i := 1 to v[(j-1) mod k + 1] do inc(a[i]);

        end;

        writeln(ans);

    end;
end.

```
