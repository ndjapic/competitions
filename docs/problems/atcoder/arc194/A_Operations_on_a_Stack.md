# Problem: A_Operations_on_a_Stack.pas

```pascal
program A_Operations_on_a_Stack;
uses
    math;
const
    nn = 200 * 1000;
var
    n, i: int32;
    a: array [1 .. nn] of int32;
    dp: array [0 .. nn] of int64;

begin
    readln(n);

    dp[0] := 0;
    read(a[1]);
    dp[1] := a[1];

    for i := 2 to n do begin
        read(a[i]);
        dp[i] := max(dp[i-2], dp[i-1] + a[i]);
    end;
    readln;

    writeln(dp[n]);
end.

```
