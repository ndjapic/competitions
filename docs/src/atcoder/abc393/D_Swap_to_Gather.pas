program D_Swap_to_Gather;
{$MODE DELPHI}
uses
    math;
const
    nn = 500 * 1000;
var
    n, i: int32;
    ans: int64;
    s: string;
    c: array [0 .. nn] of int32;

begin
    readln(n);
    readln(s);

    c[0] := 0;
    for i := 1 to n do
        c[i] := c[i-1] + ord(s[i]) - ord('0');

    ans := 0;
    for i := 1 to n do
        if s[i] = '0' then
            inc(ans, min(c[i], c[n] - c[i]));

    writeln(ans);
end.
