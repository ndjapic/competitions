# Задатак: C_Sum_of_Numbers_Greater_Than_Me.pas

```pascal
program C_Sum_of_Numbers_Greater_Than_Me;
const
    maxn = 200 * 1000;
var
    n, i, j: int32;
    a, p, merge: array [1 .. maxn] of int32;
    s, ans: array [0 .. maxn] of int64;

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
            if (ir > r) or (il <= m) and (a[p[il]] >= a[p[ir]]) then begin
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
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        p[i] := i;
    end;
    readln;
    msort(1, n);

    s[0] := 0;
    j := 1;
    for i := 1 to n do begin
        s[i] := s[i-1] + a[p[i]];
        while (j <= n) and (a[p[j]] > a[p[i]]) do inc(j);
        ans[p[i]] := s[j-1];
    end;

    for i := 1 to n-1 do write(ans[i], ' ');
    writeln(ans[n]);
end.

```
