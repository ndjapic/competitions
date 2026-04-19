# Problem: D_Many_Segments_2.pas

```pascal
program D_Many_Segments_2;
{$mode objfpc}{$h+}{$j-}{$inline on}
uses
    math;
const
    nn = 200 * 1000;
var
    n, m, i, l, r: int32;
    ans: int64;
    mn: array [0 .. nn] of int32;

begin
    readln(n, m);

    for l := 0 to m do mn[l] := m+1;

    for i := 1 to n do begin
        readln(l, r);
        mn[l] := min(mn[l], r);
    end;

    ans := 0;
    for l := m downto 1 do begin
        inc(ans, m+1-mn[l]);
        mn[l-1] := min(mn[l-1], mn[l]);
    end;

    writeln(int64(m+1) * m div 2 - ans);
end.

```
