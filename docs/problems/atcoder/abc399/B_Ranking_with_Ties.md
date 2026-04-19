# Problem: B_Ranking_with_Ties.pas

```pascal
program B_Ranking_with_Ties;
var
    n, i: int8;
    p, q, cp, ans: array of int8;

procedure msort(var arr: array of int8; l, r: int32);
var
    m, i, il, ir: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        il := l;
        ir := m;
        for i := l to r-1 do
            if (ir >= r) or (il < m) and (p[arr[il]] >= p[arr[ir]]) then begin
                cp[i] := arr[il];
                inc(il);
            end else begin
                cp[i] := arr[ir];
                inc(ir);
            end;

        for i := l to r-1 do arr[i] := cp[i];

    end;
end;

begin
    readln(n);
    setlength(p, n);
    setlength(q, n);
    setlength(cp, n);
    setlength(ans, n);

    for i := 0 to n-1 do begin
        read(p[i]);
        q[i] := i;
    end;
    readln;
    msort(q, 0, n);

    ans[q[0]] := 1;
    for i := 1 to n-1 do
        if p[q[i]] = p[q[i-1]] then
            ans[q[i]] := ans[q[i-1]]
        else
            ans[q[i]] := i+1;

    for i := 0 to n-1 do writeln(ans[i]);
end.

```
