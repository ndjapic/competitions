program E_Digit_Sum_Divisible;
const
    sq = 10 * 1000 * 1000;
    maxds = 9 * 14;
var
    n, ans: int64;
    i: int8;
    s: int16;
    dsum: array [0 .. sq] of int16;
    c: array [0 .. sq, 0 .. maxds] of int32;

begin
    for i := 0 to 9 do dsum[i] := i;
    for s := 0 to maxds do c[0, s] := 0;

    for n := 1 to sq do begin
        dsum[n] := dsum[n div 10] + n mod 10;
        for s := 0 to maxds do c[n, s] := c[n-1, s];
        inc(c[n, dsum[n]]);
    end;

    readln(n);
    if n div sq = sq then
    writeln(ans);
end.
