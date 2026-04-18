# Задатак: E_Set_Meal.pas

```pascal
program E_Set_Meal;
uses
    math;
const
    maxn = 100 * 1000;
type
    tarray = array [1 .. maxn] of int32;
var
    n, m, ll, i, j, k, le, ri, mi, ans: int32;
    well: boolean;
    a, b, p, merge, len: tarray;
    bad: array [1 .. maxn] of array of int32;

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
            if (ir > r) or (il <= m) and (b[p[il]] >= b[p[ir]]) then begin
                merge[i] := p[il];
                inc(il);
            end else begin
                merge[i] := p[ir];
                inc(ir);
            end;

        for i := l to r do p[i] := merge[i];

    end;
end;

procedure msort_bad(c, l, r: int32);
var
    m, i, il, ir: int32;
begin
    if l < r then begin

        m := (l+r) div 2;
        msort_bad(c, l, m);
        msort_bad(c, m+1, r);

        il := l;
        ir := m+1;
        for i := l to r do
            if (ir > r) or (il <= m) and (bad[c][il] <= bad[c][ir]) then begin
                merge[i] := bad[c][il];
                inc(il);
            end else begin
                merge[i] := bad[c][ir];
                inc(ir);
            end;

        for i := l to r do bad[c][i] := merge[i];

    end;
end;

begin
    readln(n, m, ll);

    for i := 1 to n do begin
        read(a[i]);
        len[i] := 0;
        setlength(bad[i], 1);
    end;
    readln;

    for j := 1 to m do begin
        read(b[j]);
        p[j] := j;
    end;
    readln;
    msort(1, m);

    for k := 1 to ll do begin
        readln(i, j);
        if len[i] = length(bad[i]) then setlength(bad[i], 2 * len[i]);
        bad[i][len[i]] := j;
        inc(len[i]);
    end;

    ans := 0;
    for i := 1 to n do begin
        msort_bad(i, 0, len[i]-1);

        j := 0;
        repeat
            inc(j);
            le := -1;
            ri := len[i];

            while ri - le > 1 do begin
                mi := (le + ri) div 2;
                if p[j] < bad[i][mi] then
                    ri := mi
                else
                    le := mi;
            end;

            well := (le = -1) or (p[j] > bad[i][le]);
        until (j = m) or well;

        if well then ans := max(ans, a[i] + b[p[j]]);
    end;

    writeln(ans);
end.

```
