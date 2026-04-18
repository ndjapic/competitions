# Задатак: C_Socks_2.pas

```pascal
program C_Socks_2;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, k, i, ai, n0, n1, pairs: int32;
    mtw: int64;
    c: array [1 .. maxn] of array [0 .. 1] of int8;
    s: array [0 .. 1] of array [1 .. maxn] of int32;
    pre, suf: array [0 .. maxn] of int64;

begin
    readln(n, k);

    for i := 1 to n do begin
        c[i][0] := 1;
        c[i][1] := 1;
    end;

    for i := 1 to k do begin
        read(ai);
        dec(c[ai][i mod 2]);
    end;
    readln;

    n0 := 0;
    n1 := 0;
    for i := 1 to n do begin

        if c[i][0] > 0 then begin
            inc(n0);
            s[0][n0] := i;
        end;

        if c[i][1] > 0 then begin
            inc(n1);
            s[1][n1] := i;
        end;

    end;

    pairs := min(n0, n1);

    pre[0] := 0;
    for i := 1 to pairs do
        pre[i] := pre[i-1] + abs(s[0][i] - s[1][i]);

    n0 := 0;
    n1 := 0;
    for i := n downto 1 do begin

        if c[i][0] > 0 then begin
            inc(n0);
            s[0][n0] := i;
        end;

        if c[i][1] > 0 then begin
            inc(n1);
            s[1][n1] := i;
        end;

    end;

    suf[0] := 0;
    for i := 1 to pairs do
        suf[i] := suf[i-1] + abs(s[0][i] - s[1][i]);

    mtw := high(int64);
    for i := 0 to pairs do
        mtw := min(mtw, pre[i] + suf[pairs - i]);

    writeln(mtw);
end.

```
