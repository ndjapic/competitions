# Задатак: D_Palindrome_Shuffle.pas

```pascal
program D_Palindrome_Shuffle;
{$MODE DELPHI}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, j, l, r, m, mn, ans: int32;
    o: int8;
    s: string;
    c: array [0 .. nn] of array [0 .. 25] of int32;
    p: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(s);
        n := length(s);

        for o := 0 to 25 do c[0][o] := 0;
        for i := 1 to n do begin
            c[i] := c[i-1];
            o := ord(s[i]) - ord('a');
            inc(c[i][o]);
        end;

        ans := 0;
        for i := 1 to n do begin

            o := ord(s[i]) - ord('a');
            l := 0;
            r := n;
            while r-l > 1 do begin

                m := (l+r) div 2;
                (* c[i][o] = 2 = 2 = c[n][o] - c[l][o] *)
                (* c[i][o] = 2 > 1 = c[n][o] - c[r][o] *)
                if c[i][o] > c[n][o] - c[m][o] then
                    r := m
                else
                    l := m;

            end;
            {p[r] := n+1-i;}
            p[i] := n+1-r;

        end;

        ans := 0;
        for i := 1 to n do begin
            mn := n+1;
            if p[i] <> i then mn := min(mn, abs(p[i] - i) + 1);
            j := n+1-i;
            if p[j] <> j then mn := min(mn, abs(p[j] - j) + 1);
            if mn < n+1 then
                ans := max(ans, mn);
        end;

        writeln(ans);

    end;
end.

```
