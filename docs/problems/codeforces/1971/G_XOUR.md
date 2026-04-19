# Problem: G_XOUR.pas

```pascal
program G_XOUR;
const
    nn = 200 * 1000;
type
    tarr = array [1 .. nn] of int32;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    a, b, p, q, merge: tarr;

procedure msort(var a, p: tarr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(a, p, l, m);
        msort(a, p, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (a[p[j]] <= a[p[k]]) then begin
                merge[i] := p[j];
                inc(j);
            end else begin
                merge[i] := p[k];
                inc(k);
            end;

        for i := l to r-1 do p[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do begin
            read(a[i]);
            b[i] := a[i] shr 2;
            p[i] := i;
        end;
        readln;

        msort(b, p, 1, n+1);

        for i := 1 to n do q[p[i]] := i;

        l := 1;
        for r := 1 to n do
            if (r = n) or (b[p[r]] < b[p[r+1]]) then begin
                msort(a, p, l, r+1);
                l := r+1;
            end;

        for i := 1 to n-1 do write(a[p[q[i]]], ' '); writeln(a[p[q[n]]]);

    end;
end.

```
