# Problem: D_Seraphim_the_Owl.pas

```pascal
program D_Seraphim_the_Owl;
uses
    math;
const
    sz = 200 * 1000 + 1;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, m, i: int32;
    ans: int64;
    a, b, mn: array [0 .. sz] of int32;
    s: array [0 .. sz] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(b[i]); readln;

        for i := 1 to n do mn[i] := min(a[i], b[i]);

        s[n+1] := 0;
        for i := n downto 1 do
            s[i] := s[i+1] + mn[i];

        ans := inf;
        for i := 1 to m do
            ans := min(ans, a[i] + s[i+1]);

        writeln(ans);

    end;
end.

```
