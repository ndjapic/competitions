program D_Grid_Puzzle;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, j: int32;
    a, b, c2, dp: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        dp[0] := 0;
        c2[0] := 0;

        for i := 1 to n do begin

            read(a[i]);

            b[i] := (a[i] + 1) div 2;
            if b[i] = 2 then
                c2[i] := c2[i-1] + 1
            else
                c2[i] := 0;

            if a[i] = 0 then
                dp[i] := dp[i-1]
            else begin

                dp[i] := dp[i-1] + 1;
                j := i-1-c2[i-1];

                if (j > 0) and odd(i+j) and (b[i] = 1) and (b[j] = 1) then
                    dp[i] := min(dp[i], dp[j-1] + i-j);

            end;

        end;
        readln;

        writeln(dp[n]);

    end;
end.
