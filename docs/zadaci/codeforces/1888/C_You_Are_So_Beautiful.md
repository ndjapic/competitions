# Задатак: C_You_Are_So_Beautiful.pas

```pascal
program C_You_Are_So_Beautiful;
uses
    math;
const
    maxn = 100 * 1000;
var
    ntc, tci, n, i, l, r, nf: int32;
    ans: int64;
    neq: boolean;
    a, p, merge: array [1 .. maxn] of int32;
    is_first, is_last: array [1 .. maxn] of boolean;

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
                (a[p[il]] < a[p[ir]]) or
                (a[p[il]] = a[p[ir]]) and
                (p[il] < p[ir])
            ) then begin
                merge[i] := p[il];
                inc(il);
            end else begin
                merge[i] := p[ir];
                inc(ir);
            end;

        for i := l to r do p[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
            p[i] := i;
        end;
        readln;
        msort(1, n);

        is_first[p[1]] := true;
        is_last[p[n]] := true;

        for i := 2 to n do begin
            l := p[i-1];
            r := p[i];
            neq := a[l] < a[r];
            is_first[r] := neq;
            is_last[l] := neq;
        end;

        nf := 0;
        ans := 0;
        for i := 1 to n do begin
            if is_first[i] then inc(nf);
            if is_last[i] then inc(ans, nf);
        end;

        writeln(ans);

    end;
end.

```
