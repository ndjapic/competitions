# Problem: A_Too_Min_Too_Max.pas

```pascal
program A_Too_Min_Too_Max;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, i: int8;
    a, merge: array [1 .. maxn] of int32;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

function d(i, j: int8): int32;
begin
    d := abs(a[i] - a[j]);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;
        msort(1, n+1);

        writeln(
            d(1, n) +
            d(2, n) +
            d(2, n-1) +
            d(1, n-1)
        );

    end;

end.

```
