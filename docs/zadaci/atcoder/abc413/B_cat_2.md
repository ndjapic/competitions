# Задатак: B_cat_2.pas

```pascal
program B_cat_2;
{$MODE DELPHI}
const
    nn = 100;
    nnnn = nn * nn;
var
    n, i, j, x: int8;
    ans, k: int16;
    s: array [1 .. nn] of string;
    t: array [1 .. nnnn] of string;
    p, cp: array [1 .. nnnn] of int16;

procedure msort(l, r: int16);
var
    m, i, il, ir: int16;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (t[p[il]] <= t[p[ir]]) then begin
                cp[i] := p[il];
                inc(il);
            end else begin
                cp[i] := p[ir];
                inc(ir);
            end;

        for i := l to r-1 do p[i] := cp[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do readln(s[i]);

    k := 0;
    for i := 1 to n do
        for j := 1 to n do
            if i <> j then begin
                inc(k);
                setlength(t[k], length(s[i]) + length(s[j]));
                for x := 1 to length(s[i]) do
                    t[k][x] := s[i][x];
                for x := 1 to length(s[j]) do
                    t[k][x + length(s[i])] := s[j][x];
                p[k] := k;
            end;
    msort(1, n*(n-1)+1);

    ans := 1;
    for k := 2 to n*(n-1) do
        if t[p[k-1]] < t[p[k]] then inc(ans);
    writeln(ans);
end.

```
