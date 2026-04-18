# Задатак: D_Creating_a_Schedule.pas

```pascal
program D_Creating_a_Schedule;
uses
    math;
const
    nn = 100 * 1000;
var
    ntc, tci, n, m, i, j, k: int32;
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

        readln(n, m);

        for j := 1 to m do read(a[j]); readln;
        msort(1, m+1);

        for i := 1 to n do begin
            for k := 1 to 6 do begin

                if odd(i+k) then
                    j := 1 + (i-1) div 2
                else
                    j := m - (i-1) div 2;

                write(a[j]);
                if k < 6 then write(' ');

            end;
            writeln;
        end;

    end;
end.

```
