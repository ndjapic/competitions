program A_Median_of_an_Array;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, m: int32;
    a, merge: array [1 .. maxn] of int32;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        m := (n+1) div 2;

        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n+1);

        i := m;
        while (i <= n) and (a[i] = a[m]) do inc(i);

        writeln(i-m);

    end;
end.
