program D_Inaccurate_Subsequence_Search;
uses
    math;
const
    maxn = 200 * 1000;
    maxx = 1000 * 1000;
var
    ntc, tci: int16;
    n, m, k, i, x, l, r, have, ans: int32;
    a, b: array [1 .. maxn] of int32;
    ca, cb: array [1 .. maxx] of int32;

begin
    for x := 1 to maxx do begin
        ca[x] := 0;
        cb[x] := 0;
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);
        for i := 1 to n do read(a[i]); readln;
        for i := 1 to m do read(b[i]); readln;

        for i := 1 to m do inc(cb[b[i]]);

        l := 1;
        r := 0;
        have := 0;
        ans := 0;

        while l <= n do
            if (r < n) and (r-l+1 < m) then begin
                inc(r);
                x := a[r];
                dec(have, min(ca[x], cb[x]));
                inc(ca[x]);
                inc(have, min(ca[x], cb[x]));
            end else begin
                if (r-l+1 = m) and (have >= k) then inc(ans);
                x := a[l];
                dec(have, min(ca[x], cb[x]));
                dec(ca[x]);
                inc(have, min(ca[x], cb[x]));
                inc(l);
            end;

        for i := 1 to m do dec(cb[b[i]]);

        writeln(ans);

    end;
end.
