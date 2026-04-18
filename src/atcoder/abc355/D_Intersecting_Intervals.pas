program D_Intersecting_Intervals;
const
    nn = 500 * 1000;
type
    tarr = array [1 .. nn] of int32;
var
    n, i, j: int32;
    ans: int64;
    l, r, a, b, merge: tarr;

procedure msort(var arr: tarr; l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(arr, l, m);
        msort(arr, m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (
                arr[j] <= arr[k]
            ) then begin
                merge[i] := arr[j];
                inc(j);
            end else begin
                merge[i] := arr[k];
                inc(k);
            end;

        for i := l to r-1 do arr[i] := merge[i];

    end;
end;

begin
    readln(n);

    for i := 1 to n do begin
        readln(l[i], r[i]);
        a[i] := l[i];
        b[i] := r[i];
    end;

    msort(a, 1, n+1);
    msort(b, 1, n+1);

    ans := int64(n-1) * n;

    j := 1;
    for i := 1 to n do begin
        while (j <= n) and (a[j] <= b[i]) do inc(j);
        dec(ans, n+1-j);
    end;

    j := n;
    for i := n downto 1 do begin
        while (j > 0) and (a[i] <= b[j]) do dec(j);
        dec(ans, j);
    end;

    writeln(ans div 2);
end.
