program B_Forming_Triangles;
const
    maxn = 300 * 1000;
var
	ntc, tci: int16;
    n, i, t, s: int32;
    ans: int64;
    a, merge: array [1 .. maxn] of int32;
    c: array [0 .. maxn] of int32;

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

function ncr(n: int64; r: int8): int64;
begin
    if r = 0 then
        ncr := 1
    else
        ncr := ncr(n-1, r-1) * n div r;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n+1);

        c[1] := 1;
        t := 1;

        for i := 2 to n do begin
            if a[i-1] < a[i] then begin
                inc(t);
                c[t] := 0;
            end;
            inc(c[t]);
        end;

        s := 0;
        ans := 0;

        for i := 1 to t do begin
            inc(ans, ncr(c[i], 3) + ncr(c[i], 2) * s);
            inc(s, c[i]);
        end;

        writeln(ans);

    end;
end.
