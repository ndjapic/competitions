# Задатак: B_Points_and_Minimum_Distance.pas

```pascal
program B_Points_and_Minimum_Distance;
const
    max2n = 200;
var
    ntc, tci, n, i, d: int16;
    a, cp: array [1 .. max2n] of int16;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (a[il] <= a[ir]) then begin
                cp[i] := a[il];
                inc(il);
            end else begin
                cp[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := cp[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to 2*n do read(a[i]); readln;
        msort(1, 2*n);

        d := 0;
        for i := 2 to n do
            inc(d, abs(a[i] - a[i-1]) + abs(a[n+i] - a[n+i-1]));

        writeln(d);
        for i := 1 to n do writeln(a[i], ' ', a[n+i]);

    end;
end.

```
