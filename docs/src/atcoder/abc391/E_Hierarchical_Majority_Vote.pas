program E_Hierarchical_Majority_Vote;
{$mode delphi}{$inline on}
uses
    math;
const
    nn = 13;
    ii = 2100 * 1000;
var
    n, e: int8;
    i, j, mx: int32;
    a: array [0 .. nn] of string;
    c: array ['0' .. '1'] of int8;
    dp: array [0 .. nn] of array [1 .. ii] of int32;

begin
    readln(n);
    readln(a[n]);

    for i := 1 to length(a[n]) do dp[n][i] := 1;

    for e := n-1 downto 0 do begin
        setlength(a[e], length(a[e+1]) div 3);
        for i := 1 to length(a[e]) do begin

            c['0'] := 0;
            c['1'] := 0;
            for j := 3*i-2 to 3*i do inc(c[a[e+1][j]]);

            if c['0'] > c['1'] then
                a[e][i] := '0'
            else
                a[e][i] := '1';

            dp[e][i] := 0;
            mx := 0;
            for j := 3*i-2 to 3*i do
                if a[e][i] = a[e+1][j] then begin
                    inc(dp[e][i], dp[e+1][j]);
                    mx := max(mx, dp[e+1][j]);
                end;

            dec(dp[e][i], mx);
            {write(dp[e][i], ' ');}

        end;
        {writeln(' | ', a[e]);}
    end;

    writeln(dp[0][1]);
end.
