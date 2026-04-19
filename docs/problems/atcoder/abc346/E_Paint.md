# Problem: E_Paint.pas

```pascal
program E_Paint;
uses
    math;
const
    maxm = 200 * 1000;
var
    h, w, m, i, n, k: int32;
    t: array [1 .. maxm] of int8;
    a, x: array [1 .. maxm] of int32;
    c: array [0 .. maxm] of int64;
    seen: array [1 .. 2] of array [1 .. maxm] of boolean;

begin
    readln(h, w, m);

    for i := 1 to m do readln(t[i], a[i], x[i]);

    for i := 0 to maxm do c[i] := 0;

    for i := 1 to h do seen[1][i] := false;
    for i := 1 to w do seen[2][i] := false;
    n := w;

    for i := m downto 1 do
        if not seen[t[i]][a[i]] then begin
            case t[i] of
                1: inc(c[x[i]], n);
                2: dec(n);
            end;
            seen[t[i]][a[i]] := true;
        end;

    for i := 1 to h do seen[1][i] := false;
    for i := 1 to w do seen[2][i] := false;
    n := h;

    for i := m downto 1 do
        if not seen[t[i]][a[i]] then begin
            case t[i] of
                1: dec(n);
                2: inc(c[x[i]], n);
            end;
            seen[t[i]][a[i]] := true;
        end;

    for i := 1 to w do
        if not seen[2][i] then
            inc(c[0], n);

    k := 0;
    for i := 0 to maxm do
        if c[i] > 0 then inc(k);
    writeln(k);

    for i := 0 to maxm do
        if c[i] > 0 then writeln(i, ' ', c[i]);
end.

```
