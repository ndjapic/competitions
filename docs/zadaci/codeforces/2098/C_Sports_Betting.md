# Задатак: C_Sports_Betting.pas

```pascal
program C_Sports_Betting;
type
    tarr = array of int32;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    a, cp: tarr;
    found: boolean;

procedure msort(var arr: tarr; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (arr[il] <= arr[ir]) then begin
                cp[i] := arr[il];
                inc(il);
            end else begin
                cp[i] := arr[ir];
                inc(ir);
            end;

        for i := l to r-1 do arr[i] := cp[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        setlength(a, n);
        setlength(cp, n);

        for i := 1 to n do read(a[i-1]); readln;
        msort(a, 0, n);

        l := 2;
        found := false;
        for r := 2 to n-1 do begin
            if a[r-1] - a[l-1] <> r-l then l := r;
            if not found and (l < r) then
                found := (a[l-2] = a[l-1]) and (a[r-1] = a[r]);
            if not found and (2 < r) then
                found := a[r-3] = a[r];
        end;

        if found then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
