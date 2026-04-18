# Задатак: B_Subsegments_with_Small_Sums.pas

```pascal
program B_Subsegments_with_Small_Sums;
const
    maxn = 250 * 1000;
var
    n, l, r: int32;
    s, ans: int64;
    a, dp: array [0 .. maxn] of int64;

begin
    readln(n, s);
    a[0] := 0;
    dp[0] := 0;
    ans := 0;
    l := 1;

    for r := 1 to n do begin

        read(a[r]);
        inc(a[r], a[r-1]);

        while a[r] - a[l-1] > s do inc(l);
        dp[r] := r + dp[l-1];
        inc(ans, dp[r]);

    end;
    readln;

    writeln(ans);
end.

```
