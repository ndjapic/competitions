# Задатак: F_Equal_XOR_Segments.pas

```pascal
program F_Equal_XOR_Segments;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, q, i, l, r, h, lo, hi, m, l2, r2: int32;
    a, x, p, merge, s, height: array [0 .. nn] of int32;

procedure msorti(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msorti(l, m);
        msorti(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                x[p[j]] <= x[p[k]]
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
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, q);

        x[0] := 0;
        p[0] := 0;
        for i := 1 to n do begin
            read(a[i]);
            x[i] := x[i-1] xor a[i];
            p[i] := i;
        end;

        msorti(0, n+1);

        h := 0;
        s[0] := -1;
        for i := 0 to n do begin
            if (i = 0) or (x[p[i]] > x[p[i-1]]) then begin
                inc(h);
                s[h] := i;
            end;
            height[p[i]] := h;
        end;
        s[h+1] := n+1;

        for i := 1 to q do begin

            readln(l, r);
            dec(l);

            h := height[l];
            lo := s[h];
            hi := s[h+1];
            while hi-lo > 1 do begin
                m := (lo+hi) div 2;
                if p[m] > r then
                    hi := m
                else
                    lo := m;
            end;
            r2 := p[lo];

            h := height[r];
            lo := s[h]-1;
            hi := s[h+1]-1;
            while hi-lo > 1 do begin
                m := (lo+hi) div 2;
                if p[m] < l then
                    lo := m
                else
                    hi := m;
            end;
            l2 := p[hi];

            if l2 < r2 then
                writeln('YES')
            else
                writeln('NO');

        end;

        if tci < ntc then writeln;

    end;
end.

```
