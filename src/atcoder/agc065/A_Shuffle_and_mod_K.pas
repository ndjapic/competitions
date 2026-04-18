program A_Shuffle_and_mod_K;
uses
    math;
const
    maxn = 200 * 1000;
var
    n, i, k, nu: int32;
    a, merge, uni, c: array [1 .. maxn] of int32;

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
            if (ir > r) or (il <= m) and (a[il] <= a[ir]) then begin
                merge[i] := a[il];
                inc(il);
            end else begin
                merge[i] := a[ir];
                inc(ir);
            end;

        for i := l to r do a[i] := merge[i];

    end;
end;

begin
    readln(n, k);

    for i := 0 to n-1 do read(a[i]);
    readln;
    msort(1, n);

    uni[1] := a[1];
    c[1] := 1;
    nu := 1;

    for i := 2 to n do begin
        if a[i] > a[i-1] then begin
            inc(nu);
            uni[nu] := a[i];
            c[nu] := 0;
        end;
        inc(c[nu]);
    end;

    i := 1;
    while (i < nu) and (b[i+1] - b[i] < k - b[i+1] - b[i]) do inc(i);

    writeln();
end.
