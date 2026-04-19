# Problem: B_Make_Many_Triangles.pas

```pascal
program B_Make_Many_Triangles;
uses
    math;
const
    maxn = 300;
var
    n, i, j, k: int16;
    x, y: array [1 .. maxn] of int32;
    dp: array [0 .. maxn] of int64;

function collinear(i, j, k: int16): boolean;
begin
    {if x[i] = x[j] then
        colinear := x[j] = x[k]
    else if x[j] = x[k] then
        colinear := x[k] = x[i]
    else if x[k] = x[i] then
        colinear := x[i] = x[j]
    else}
        collinear := int64(y[j] - y[i]) * (x[k] - x[j]) = int64(y[k] - y[j]) * (x[j] - x[i]);
end;

begin
    readln(n);

    dp[0] := 0;

    for i := 1 to n do begin

        readln(x[i], y[i]);
        dp[i] := dp[i-1];

        for j := 1 to i-1 do
            for k := 1 to j-1 do begin
                if not collinear(i, j, k) then
                    dp[i] := max(dp[i], dp[k-1] + dp[j-1] - dp[k] + dp[i-1] - dp[j] + 1);
            end;

    end;

    writeln(dp[n]);
end.

```
