# Problem: F_Variety_Split_Hard.pas

```pascal
program F_Variety_Split_Hard;
uses
    math;
const
    nn = 300 * 1000;
var
    n, i, x, ans: int32;
    a, prev, last, s: array [1 .. nn] of int32;
    pre, suf: array [0 .. nn] of int32;

begin
    readln(n);

    for x := 1 to n do last[x] := 0;

    for i := 1 to n do begin

        read(x);
        a[i] := x;

        s[i] := 0;
        if last[x] > 0 then begin
            inc(s[last[x]]);
            dec(s[i]);
        end;

        prev[i] := last[x];
        last[x] := i;

    end;
    readln;

    for i := 2 to n do inc(s[i], s[i-1]);

    pre[1] := s[1];
    suf[n] := s[n];

    for i := 2 to n do pre[i] := max(pre[i-1], s[i]);
    for i := n-1 downto 1 do suf[i] := max(suf[i+1], s[i]);

    ans := 0;
    for i := 1 to n-1 do
        ans := max(ans, pre[i] + suf[i+1]);

    writeln(n-2+ans);
end.

```
