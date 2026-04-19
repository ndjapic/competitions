# Problem: B_Substring_and_Subsequence.pas

```pascal
program B_Substring_and_Subsequence;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    ntc, tci: int16;
    a, b: string;
    n, m, i, l, r: int8;
    ans: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a); n := length(a);
        readln(b); m := length(b);

        ans := n+m;

        for l := 1 to m do begin
            r := l;
            for i := 1 to n do
                if (r <= m) and (a[i] = b[r]) then inc(r);
            ans := min(ans, n+m-(r-l));
        end;

        writeln(ans);

    end;
end.

```
