# Problem: F_Multiplicative_Arrays.pas

```pascal
program F_Multiplicative_Arrays;
{$mode delphi}{$inline on}
uses
    math;
const
    kk = 100 {* 1000};
    nn = 100 {* 1000};
    prime = 998244353;
var
    ntc, tci: int16;
    n, k, x, y: int32;
    dp: array [1 .. kk, 1 .. kk, 0 .. nn] of int32;

begin
    for k := 1 to kk do
        for x := 1 to k do
            dp[x, k, 0] := 1;

    for n := 1 to nn do begin
        for k := 1 to kk do
            for x := 1 to k do begin
                dp[x, k, n] := 1;
                y := x;
                while y <= k do begin
                    inc(dp[x, k, n], dp[y, k, n-1]);
                    inc(y, x);
                end;
                dp[x, k, n] := dp[x, k, n] mod prime;
            end;
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(k, n);

        for x := 1 to k do
            write(dp[x, x, n] - dp[x-1, x-1, n], ' ');
        writeln;

    end;
end.
https://codeforces.com/contest/2060/problem/F

```
