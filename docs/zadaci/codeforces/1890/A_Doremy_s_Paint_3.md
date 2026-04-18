# Задатак: A_Doremy_s_Paint_3.pas

```pascal
program A_Doremy_s_Paint_3;
uses
    math;
const
    maxn = 100;
var
    ntc, tci, n, i: int8;
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
            if (ir > r) or (il <= m) and (
                a[il] <= a[ir]
            ) then begin
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
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n);

        i := 1;
        while (i < n div 2) and (a[i] = a[i+1]) and (a[n-i] = a[n-i+1]) do inc(i);

        if i < n div 2 then
            writeln('No')
        else if not odd(n) then
            writeln('Yes')
        else if a[i] = a[i+1] then
            writeln('Yes')
        else if a[i+2] = a[i+1] then
            writeln('Yes')
        else
            writeln('No');

    end;
end.

```
