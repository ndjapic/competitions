# Задатак: D_Quartet_Swapping.pas

```pascal
program D_Quartet_Swapping;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, h, x: int32;
    a, inv, s, cp, b: array [1 .. nn] of int32;

procedure swp(i, j: int32);
var
    x: int32;
begin
    x := a[i];
    a[i] := a[j];
    a[j] := x;

    x := a[i+1];
    a[i+1] := a[j+1];
    a[j+1] := x;

    inv[a[i]] := i;
    inv[a[j]] := j;
    inv[a[i+1]] := i+1;
    inv[a[j+1]] := j+1;
end;

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
        h := n div 2;

        for i := 1 to n do begin
            read(a[i]);
            inv[a[i]] := i;
        end;
        readln;

        for i := 1 to n-h do s[h+i] := a[2*i-1];
        for i := 1 to h do s[i] := a[2*i];

        msort(1, h+1);
        msort(h+1, n+1);

        for i := 1 to n-h do b[2*i-1] := s[h+i];
        for i := 1 to h do b[2*i] := s[i];

        for i := 1 to n-3 do begin
            x := b[i];
            if inv[x] = n then swp(n-3, n-1);
            if i <> inv[x] then swp(i, inv[x]);
        end;

        for i := 1 to n-1 do write(a[i], ' ');
        writeln(a[n]);

    end;
end.

```
