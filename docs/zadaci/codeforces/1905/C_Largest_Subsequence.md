# Задатак: C_Largest_Subsequence.pas

```pascal
program C_Largest_Subsequence;
{$H+}
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, c, m: int32;
    s, t: string;
    p, merge, a: array [1 .. maxn] of int32;

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
                (s[p[il]] >= s[p[ir]])
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
        readln(s);
        for i := 1 to n do p[i] := i;
        msort(1, n);

        m := 1;
        c := 1;
        a[1] := p[1];

        for i := 2 to n do
            if p[i] > a[c] then begin
                inc(c);
                a[c] := p[i];
                if s[a[c]] = s[a[1]] then m := i;
            end;

        t := s;
        for i := 1 to m do t[a[i+c-m]] := s[a[i]];
        for i := m+1 to c do t[a[c-i+1]] := s[a[i]];

        i := 1;
        while (i < n) and (t[i] <= t[i+1]) do inc(i);

        if i < n then
            writeln(-1)
        else
            writeln(c-m);

    end;
end.

```
