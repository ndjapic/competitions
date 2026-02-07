program D_Vlad_and_Division;
const
    maxn = 200 * 1000;
    m = high(int32);
var
    ntc, tci, n, i, l, r, ans: int32;
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
        for i := 1 to n do read(a[i]); readln;
        msort(1, n+1);

        ans := 0;
        l := 1;
        r := n;
        while l <= r do begin
            if a[l] < m - a[r] then
                inc(l)
            else if a[l] > m - a[r] then
                dec(r)
            else begin
                inc(l);
                dec(r);
            end;
            inc(ans);
        end;

        writeln(ans);

    end;
end.
