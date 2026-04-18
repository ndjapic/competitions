# Задатак: B_Apples_in_Boxes.pas

```pascal
program B_Apples_in_Boxes;
uses
    math;
const
    nn = 100 * 1000;
var
    ntc, tci: int16;
    n, k, i, mx: int32;
    s: int64;
    a, cp: array [1 .. nn] of int32;

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
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, k);

        s := 0;
        for i := 1 to n do begin
            read(a[i]);
            inc(s, a[i]);
        end;
        readln;
        msort(1, n+1);

        mx := a[n] - 1;
        if n > 1 then mx := max(mx, a[n-1]);

        if odd(s) and (mx - a[1] <= k) then
            writeln('Tom')
        else
            writeln('Jerry');

    end;
end.

```
