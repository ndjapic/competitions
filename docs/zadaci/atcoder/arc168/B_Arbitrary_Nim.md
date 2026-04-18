# Задатак: B_Arbitrary_Nim.pas

```pascal
program B_Arbitrary_Nim;
const
    maxn = 250 * 1000;
var
    n, i, x: int32;
    a, merge: array [1 .. maxn] of int32;

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
                merge[i] := a[il];
                inc(il);
            end else begin
                merge[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

begin
    readln(n);
    x := 0;

    for i := 1 to n do begin
        read(a[i]);
        x := x xor a[i];
    end;
    readln;

    if x > 0 then
        writeln(-1)
    else begin

        msort(1, n);
        while (n > 1) and (a[n] = a[n-1]) do dec(n, 2);

        if n = 0 then
            writeln(0)
        else
            writeln(a[n] - 1);

    end;
end.

```
