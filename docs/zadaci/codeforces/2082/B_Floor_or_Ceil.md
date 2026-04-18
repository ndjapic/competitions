# Задатак: B_Floor_or_Ceil.pas

```pascal
program B_Floor_or_Ceil;
uses
    math;
const
    nn = 30;
var
    ntc, tci: int16;
    x, n, m, i, j, mn, mx: int32;
    dp: array [0 .. nn, 0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(x, n, m);

        n := min(n, 30);
        m := min(m, 30);

        dp[0, 0] := x;
        for i := 1 to n do dp[i, 0] := dp[i-1, 0] div 2;
        for j := 1 to m do dp[0, j] := (dp[0, j-1] + 1) div 2;

        for i := 1 to n do
            for j := 1 to m do
                dp[i, j] := min(
                    dp[i-1, j] div 2,
                    (dp[i, j-1] + 1) div 2
                );

        mn := dp[n, m];

        for i := 1 to n do
            for j := 1 to m do
                dp[i, j] := max(
                    dp[i-1, j] div 2,
                    (dp[i, j-1] + 1) div 2
                );

        mx := dp[n, m];

        writeln(mn, ' ', mx);

    end;

end.

```
