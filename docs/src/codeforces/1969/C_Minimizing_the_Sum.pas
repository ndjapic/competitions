program C_Minimizing_the_Sum;
uses
    math;
const
    sz = 300 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    k, j, d: int8;
    mn: int64;
    a: array [1 .. sz] of int32;
    dp: array [0 .. sz, 0 .. 10] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);
        for i := 1 to n do read(a[i]); readln;
        for d := 0 to k do dp[0, d] := 0;

        for i := 1 to n do
            for j := 0 to k do begin
                mn := inf;
                dp[i, j] := inf;
                for d := 0 to min(i-1, j) do begin
                    mn := min(mn, a[i-d]);
                    dp[i, j] := min( dp[i, j], dp[i-d-1, j-d] + mn * (d+1) );
                end;
                if j > 0 then dp[i, j] := min(dp[i, j], dp[i, j-1]);
            end;

        writeln(dp[n, k]);

    end;
end.
