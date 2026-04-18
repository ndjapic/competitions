# Задатак: C_Variety_Split_Easy.pas

```pascal
program C_Variety_Split_Easy;
uses
    math;
const
    nn = 300 * 1000;
var
    n, i, x, ans: int32;
    a, c: array [1 .. nn] of int32;
    pre, suf: array [0 .. nn] of int32;

begin
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        pre[i] := 0;
        suf[i] := 0;
    end;
    readln;

    for x := 1 to n do c[x] := 0;

    for i := 1 to n do begin
        pre[i] := pre[i-1];
        x := a[i];
        if c[x] = 0 then inc(pre[i]);
        inc(c[x]);
    end;

    for x := 1 to n do c[x] := 0;

    for i := n-1 downto 0 do begin
        suf[i] := suf[i+1];
        x := a[i+1];
        if c[x] = 0 then inc(suf[i]);
        inc(c[x]);
    end;

    ans := 0;
    for i := 1 to n-1 do
        ans := max(ans, pre[i] + suf[i]);

    writeln(ans);
end.

```
