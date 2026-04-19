# Problem: B_Citation.pas

```pascal
program B_Citation;
const
    nn = 101;
    inf = 1000 * 1000 * 1000 + 1;
var
    n, i: int8;
    a, cp: array [0 .. nn] of int32;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (a[il] <= a[ir]) then begin
                cp[i] := a[il];
                inc(il);
            end else begin
                cp[i] := a[ir];
                inc(ir);
            end;

        for i := l to r-1 do a[i] := cp[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do read(a[i]);
    readln;
    msort(1, n+1);

    a[0] := 0;
    a[n+1] := inf;

    i := 0;
    while a[i] < n+1-i do inc(i);

    writeln(n+1-i);
end.

```
