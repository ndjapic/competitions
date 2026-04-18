# Задатак: F_Minimize_Bounding_Square.pas

```pascal
program F_Minimize_Bounding_Square;
uses
    math;
const
    maxn = 200 * 1000;
    maxx = 1000 * 1000 * 1000;
type
    tarray = array [0 .. maxn] of int64;
var
    n, i, l, r, m: int32;
    k: int64;
    x, y, merge, sx, sy: tarray;

procedure msort(var a: tarray; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(a, l, m);
        msort(a, m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (a[il] <= a[ir]) then begin
                merge[i] := a[il];
                inc(il);
            end else begin
                merge[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

function need(var a, s: tarray; m: int32): int64;
var
    l, r, mn, mx: int32;
    ans: int64;
begin
    ans := high(int64);

    r := 1;
    for l := 1 to n do begin
        while (r < n) and (a[r+1] - a[l] <= m) do inc(r);
        mn := a[l];
        mx := mn + m;
        ans := min(ans, s[n] - s[r] - int64(n-r) * mx + int64(l-1) * mn - s[l-1]);
    end;

    l := n;
    for r := n downto 1 do begin
        while (1 < l) and (a[r] - a[l-1] <= m) do dec(l);
        mx := a[r];
        mn := mx - m;
        ans := min(ans, s[n] - s[r] - int64(n-r) * mx + int64(l-1) * mn - s[l-1]);
    end;

    need := ans;
end;

begin
    readln(n, k);

    for i := 1 to n do readln(x[i], y[i]);
    msort(x, 1, n);
    msort(y, 1, n);

    sx[0] := 0;
    sy[0] := 0;
    for i := 1 to n do begin
        sx[i] := sx[i-1] + x[i];
        sy[i] := sy[i-1] + y[i];
    end;

    l := -1;
    r := maxx;
    while r - l > 1 do begin

        m := (l + r) div 2;
        if need(x, sx, m) + need(y, sy, m) <= k then
            r := m
        else
            l := m;

    end;

    writeln(r);
end.

```
