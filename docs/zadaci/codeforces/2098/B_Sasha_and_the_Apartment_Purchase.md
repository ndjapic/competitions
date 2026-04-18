# Задатак: B_Sasha_and_the_Apartment_Purchase.pas

```pascal
program B_Sasha_and_the_Apartment_Purchase;
type
    tarr = array of int32;
var
    ntc, tci: int16;
    n, i, k, l, r: int32;
    a, cp: tarr;

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

        readln(n, k);
        setlength(a, n);
        setlength(cp, n);

        for i := 1 to n do read(a[i-1]); readln;
        msort(a, 0, n);

        l := (1 + n-k) div 2;
        r := (k+1 + n + 1) div 2;

        writeln(a[r-1] - a[l-1] + 1);

    end;
end.

```
