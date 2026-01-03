program E_Battles_in_a_Row;
uses
    math;
const
    nn = 3000;
var
    n, h, m, i, j, k, l, r, ans: int32;
    a, b: array [1 .. nn] of int32;
    c: array [0 .. nn, 0 .. nn] of int32;
    bfs: array of record
        i, j: int32;
    end;

procedure enque(i, j, k: int32);
begin
    if (i >= 0) and (j >= 0) and (c[i, j] < k) then begin
        c[i, j] := k;
        if length(bfs) = r then setlength(bfs, 2*r);
        bfs[r].i := i;
        bfs[r].j := j;
        inc(r);
        ans := max(ans, k);
    end;
end;

begin
    readln(n, h, m);

    for k := 1 to n do readln(a[k], b[k]);

    for i := 0 to h do
        for j := 0 to m do c[i, j] := 0;

    l := 0;
    r := 1;
    setlength(bfs, 1);
    bfs[0].i := h;
    bfs[0].j := m;
    ans := 0;

    while l < r do begin

        i := bfs[l].i;
        j := bfs[l].j;
        k := c[i, j] + 1;
        inc(l);

        if k <= n then begin
            enque(i - a[k], j, k);
            enque(i, j - b[k], k);
        end;

    end;

    writeln(ans);
end.
