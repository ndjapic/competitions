program E_Most_Valuable_Parentheses;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, l, r: int32;
    max_score: int64;
    a: array [1 .. 2*nn] of int32;
    dp: array [1 .. nn, 1 .. 2*nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to 2*n do read(a[i]); readln;

        dp[i, r]

    end;
end.
