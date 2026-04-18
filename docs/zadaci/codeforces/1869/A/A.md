# Задатак: A.pas

```pascal
program A;
uses
    math;
const
    maxn = 100 * 1000;
var
    n, x, y, q, i, k: int32;
    j, j1: int8;
    ans: int64;
    p: array [1 .. maxn] of int8;
    t: array [1 .. maxn] of int32;
    dp: array [1 .. maxn, 0 .. 7] of int64;

begin
    readln(n, x, y);

    for i := 1 to n-1 do readln(p[i], t[i]);

    for i := n-1 downto 1 do
        for j := 0 to p[i] - 1 do begin
            dp[i, j] := t[i];
            if j > 0 then inc(dp[i, j], p[i]);
            if i < n-1 then begin
                j1 := dp[i, j] mod p[i+1];
                inc(dp[i, j], dp[i+1, j1] - j1);
            end;
        end;

    readln(q);
    for k := 1 to q do begin
        readln(ans);
        inc(ans, x);
        j := ans mod p[1];
        writeln(ans + dp[1, j] - j + y);
    end;
end.


```
