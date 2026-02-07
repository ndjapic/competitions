program B_Collatz_Problem;
{$mode delphi}
const
    nn = 100;
    aa = 1000 * 1000;
var
    n: int32;
    dp: array [1 .. aa] of int32;

function m(n: int32): int32;
begin
    if dp[n] > 0 then
    else if odd(n) then
        dp[n] := 1 + m(3*n+1)
    else
        dp[n] := 1 + m(n div 2);
    result := dp[n];
end;

begin
    for n := 1 to aa do dp[n] := 0;

    dp[1] := 4;
    dp[2] := 4;
    dp[4] := 4;

    for n := 1 to nn do dp[n] := m(n);

    readln(n);
    writeln(dp[n]);
end.
