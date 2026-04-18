program D_Flip_to_Gather;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, mn: int32;
    s: string;
    c, dp: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        c[0] := 0;
        for i := 1 to n do begin
            c[i] := c[i-1];
            if s[i] = '1' then inc(c[i]);
        end;

        dp[0] := 0;
        for i := 1 to n do begin
            dp[i] := dp[i-1];
            if s[i] = '0' then inc(dp[i]);
            dp[i] := min(dp[i], c[i]);
        end;

        mn := min(c[n], n-c[n]);
        for i := 1 to n do
            mn := min(mn, dp[i] + min(c[n] - c[i], (n-i) - (c[n] - c[i])));

        writeln(mn);
    end;
end.
