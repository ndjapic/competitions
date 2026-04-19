# Problem: B_Progressive_Square.pas

```pascal
program B_Progressive_Square;
const
    maxn = 500;
    maxnn = 500 * 500;
type
    tarr = array [1 .. maxnn] of int32;
var
    ntc, tci: int16;
    n, i, j, c, d: int32;
    a, b, merge: tarr;

function ij(i, j: int32): int32;
begin
    ij := (i-1)*n + j;
end;

procedure msort(var a: tarr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(a, l, m);
        msort(a, m, r);

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

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, c, d);

        for i := 1 to n*n do read(b[i]); readln;
        msort(b, 1, n*n+1);

        for i := 1 to n do
            for j := 1 to n do
                a[ij(i, j)] := b[1] + (i-1)*c + (j-1)*d;
        msort(a, 1, n*n+1);

        i := 1;
        while (i <= n*n) and (a[i] = b[i]) do inc(i);

        if i <= n*n then
            writeln('NO')
        else
            writeln('YES');

    end;
end.

```
