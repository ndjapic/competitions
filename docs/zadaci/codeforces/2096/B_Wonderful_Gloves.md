# Задатак: B_Wonderful_Gloves.pas

```pascal
program B_Wonderful_Gloves;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, i: int32;
    s: int64;
    l, r: array [1 .. nn] of int32;
    mn, cp: array of int32;

procedure msort(var arr: array of int32; l, r: int32);
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

        readln(n, k);

        for i := 1 to n do read(l[i]); readln;
        for i := 1 to n do read(r[i]); readln;

        s := 0;
        setlength(mn, n);
        setlength(cp, n);
        for i := 1 to n do begin
            mn[i-1] := min(l[i], r[i]);
            inc(s, l[i] + r[i]);
        end;

        msort(mn, 0, n);
        for i := 0 to n-k do dec(s, mn[i]);
        writeln(s+1);

    end;
end.

```
