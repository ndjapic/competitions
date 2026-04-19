# Problem: E_Bowls_and_Beans.pas

```pascal
program E_Bowls_and_Beans;
uses
    math;
const
    nn = 2000;
var
    n, i, j: int16;
    c, dp: array [0 .. nn] of int16;
    a: array [0 .. nn] of int8;

begin
    readln(n);

    for i := 1 to n-1 do read(c[i]); readln;
    for i := 1 to n-1 do read(a[i]); readln;

    c[n] := n;
    a[n] := 1;

    dp[0] := 0;
    for i := 1 to n do begin
        dp[i] := dp[i-1];
        j := i-1;

        while (j >= i-c[i]) and (a[j] = 0) do begin
            dp[i] := min(dp[i], dp[j]);
            dec(j);
        end;

        if j >= i-c[i] then dp[i] := dp[j];
        inc(dp[i]);
    end;

    writeln(dp[n]-1);
end.

(* https://dreampuf.github.io/GraphvizOnline/?engine=dot#digraph%20%7B%0A%20%20%20%203%20-%3E%202%20-%3E%201%20-%3E%200%0A%20%20%20%204%20-%3E%20%7B3%202%7D%0A%20%20%20%205%20-%3E%20%7Brank%3Dsame%204%203%202%201%200%7D%0A%20%20%20%207%20-%3E%206%20-%3E%205%0A%20%20%20%208%20-%3E%20%7B7%206%205%7D%0A%20%20%20%209%20-%3E%20%7B8%207%206%205%7D%0A%20%20%20%2010%20-%3E%209%0A%20%20%20%2011%20-%3E%20%7Brank%3Dsame%2010%209%208%207%7D%0A%20%20%20%2012%20-%3E%20%7B11%2010%209%7D%0A%20%20%20%2014%20-%3E%2013%20-%3E%2012%0A%20%20%20%2015%20-%3E%20%7Brank%3Dsame%2014%2013%7D%0A%20%20%20%20%0A%20%20%20%201%20%5Bstyle%3Dfilled%5D%0A%20%20%20%205%20%5Bstyle%3Dfilled%5D%0A%20%20%20%208%20%5Bstyle%3Dfilled%5D%0A%20%20%20%209%20%5Bstyle%3Dfilled%5D%0A%20%20%20%2015%20%5Bstyle%3Dfilled%5D%0A%7D%0A *)

```
