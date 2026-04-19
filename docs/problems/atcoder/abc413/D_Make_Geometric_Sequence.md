# Problem: D_Make_Geometric_Sequence.pas

```pascal
program D_Make_Geometric_Sequence;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i: int32;
    ans: boolean;
    a, cp: array [1 .. nn] of int64;

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
            if (ir >= r) or (il < m) and (
                (abs(a[il]) < abs(a[ir])) or
                (abs(a[il]) = abs(a[ir])) and
                (a[il] <= a[ir])
            ) then begin
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

        readln(n);
        for i := 1 to n do read(a[i]); readln;
        msort(1, n+1);

        if a[1] = a[n] then begin

            ans := true;

        end else if abs(a[1]) < abs(a[n]) then begin

            i := 2;
            while (i < n) and (a[i-1] * a[i+1] = a[i] * a[i]) do inc(i);
            ans := i = n;

        end else if odd(n) then begin

            i := (n+1) div 2;
            ans := a[i-1] < a[i+1];

        end else begin

            i := (n+1) div 2;
            ans := a[i] < a[i+1];

        end;

        if ans then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
