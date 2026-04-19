# Problem: D_Flower_Boy.pas

```pascal
program D_Flower_Boy;
uses
    math;
const
    nn = 200 * 1000;
    inf = 1 shl 30;
var
    ntc, tci: int16;
    n, m, i, j, ans: int32;
    a, b: array [1 .. nn] of int32;
    f, g: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);

        for i := 1 to n do read(a[i]); readln;
        for j := 1 to m do read(b[j]); readln;

        f[0] := 0;
        j := 1;
        for i := 1 to n do
            if (j <= m) and (a[i] >= b[j]) then begin
                f[i] := j;
                inc(j);
            end else
                f[i] := f[i-1];

        g[n] := 0;
        j := m;
        for i := n downto 1 do
            if (j > 0) and (a[i] >= b[j]) then begin
                dec(j);
                g[i-1] := m-j;
            end else
                g[i-1] := g[i];

        if f[n] = m then
            writeln(0)
        else begin
            ans := inf;
            for i := 0 to n do
                if f[i] + g[i] = m-1 then
                    ans := min(ans, b[f[i]+1]);
            if ans = inf then ans := -1;
            writeln(ans);
        end;

    end;
end.

```
