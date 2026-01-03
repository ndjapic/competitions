program B_Equalize;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci, n, i, nb, l, r, ans: int32;
    a, b: array [1 .. maxn] of int32;

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
                b[i] := a[j];
                inc(j);
            end else begin
                b[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := b[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln; msort(1, n+1);

        nb := 1;
        b[1] := a[1];
        for i := 2 to n do
            if a[i] > a[i-1] then begin
                inc(nb);
                b[nb] := a[i];
            end;

        l := 1;
        r := 1;
        ans := 1;

        while r <= nb do begin
            if b[r] - b[l] >= n then
                inc(l)
            else begin
                ans := max(ans, r-l+1);
                inc(r);
            end;
        end;

        writeln(ans);

    end;
end.
