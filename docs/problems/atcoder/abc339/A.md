# Problem: A.pas

```pascal
program A;
uses
	math;
const
    maxn = 200 * 1000;
var
    n, i: int32;
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
            if (k = r) or (j < m) and (
                a[j] <= a[k]
            ) then begin
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
    readln(n);

    for i := 1 to n do begin
        readln(a[i]);
    end;
    readln;

    msort(1, n+1);

    writeln('No');
end.

```
