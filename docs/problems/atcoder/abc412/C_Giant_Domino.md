# Problem: C_Giant_Domino.pas

```pascal
program C_Giant_Domino;
const
    nn = 200 * 1000;
var
    ntc, tci, n, i, j: int32;
    s, cp, a: array [1 .. nn] of int32;

procedure msort(l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (s[il] <= s[ir]) then begin
                cp[i] := s[il];
                inc(il);
            end else begin
                cp[i] := s[ir];
                inc(ir);
            end;

        for i := l to r-1 do s[i] := cp[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(s[i]);
        readln;
        msort(2, n);

        i := 1;
        j := 1;
        a[j] := s[i];

        while (i+1 < n) and (s[n] > 2*a[j]) do begin
            inc(i);
            while (i+1 < n) and (s[i+1] <= 2*a[j]) do inc(i);
            if s[i] <= 2*a[j] then begin
                inc(j);
                a[j] := s[i];
            end;
        end;

        if (i < n) and (s[n] <= 2*a[j]) then
            writeln(j+1)
        else
            writeln(-1);

    end;
end.

```
