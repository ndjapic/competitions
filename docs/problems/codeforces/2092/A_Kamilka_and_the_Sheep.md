# Problem: A_Kamilka_and_the_Sheep.pas

```pascal
program A_Kamilka_and_the_Sheep;
var
    ntc, tci: int16;
    n, i: int32;
    a, cp: array of int32;

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
    setlength(a, 100);
    setlength(cp, 100);
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 0 to n-1 do read(a[i]); readln;
        msort(a, 0, n);

        writeln(a[n-1] - a[0]);

    end;
end.

```
