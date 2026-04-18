# Задатак: D_Reindeer_and_Sleigh.pas

```pascal
program D_Reindeer_and_Sleigh;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, q, i, l, r, m: int32;
    x: int64;
    a, merge: array [0 .. maxn] of int64;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        j := l;
        k := m+1;
        for i := l to r do
            if (k > r) or (j <= m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

begin
    readln(n, q);

    for i := 1 to n do read(a[i]);
    readln;
    msort(1, n);

    a[0] := 0;
    for i := 1 to n do inc(a[i], a[i-1]);

    for i := 1 to q do begin

        readln(x);

        l := 0;
        r := n+1;
        while r - l > 1 do begin

            m := (l + r) div 2;
            if a[m] <= x then
                l := m
            else
                r := m;

        end;

        writeln(l);

    end;
end.

```
