# Problem: E_Good_Triples.pas

```pascal
program E_Good_Triples;
uses
    math;
const
    maxn = 10 * 1000 {* 1000} * 5;
var
    ntc, tci: int16;
    n, a, b: int32;
    d: int8;
    digsum: array [0 .. maxn] of int32;
    f: array [0 .. maxn, 0 .. 63] of int64;
    ans: array [0 .. maxn] of int64;

begin
    digsum[0] := 0;
    digsum[maxn] := 1;
    for a := 0 to maxn div 10 - 1 do
        for b := 0 to 9 do
            digsum[10*a+b] := digsum[a] + b;

    for n := 0 to maxn do begin
        for d := 0 to 63 do f[n, d] := 0;
        ans[n] := 0;
    end;

    for n := 0 to maxn do
        for a := 0 to n do begin
            d := digsum[a] + digsum[n-a];
            if d <= 63 then inc(f[n, d]);
        end;

    for n := 0 to maxn do
        for a := 0 to n do begin
            d := digsum[n] - digsum[a];
            if d >= 0 then inc(ans[n], f[n-a, d]);
        end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        writeln(ans[n]);

    end;
end.

```
