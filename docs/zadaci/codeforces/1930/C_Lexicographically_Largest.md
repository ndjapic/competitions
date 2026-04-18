# Задатак: C_Lexicographically_Largest.pas

```pascal
program D1_Sum_over_all_Substrings_Easy_Version;
{$H+}
uses
    math;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, i, j: int8;
    ans: int16;
    s: string;

function f(i, j: int8): int16;
var
    l, r: int8;
    ans: int16;
begin
    ans := 0;
    l := i;
    for r := i to j do
        if s[r] = '0' then
            l := r+1
        else if (r = j) or (s[r+1] = '0') then
            inc(ans, max((r-l+1) div 2, 1));
    f := ans;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        ans := 0;
        for i := 1 to n do
            for j := i to n do
                inc(ans, f(i, j));

        writeln(ans);

    end;

end.

```
