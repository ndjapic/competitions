# Problem: B_250_Thousand_Tons_of_TNT.pas

```pascal
program B_250_Thousand_Tons_of_TNT;
uses
    math;
const
    maxn = 150 * 1000;
var
    ntc, tci: int16;
    n, k, t, i, j: int32;
    w, mx, mn, ans: int64;
    a, nd: array [1 .. maxn] of int32;
    s: array [0 .. maxn] of int64;
    d: array [1 .. maxn] of array of int32;

begin
    for n := 1 to maxn do begin
        setlength(d[n], 1);
        nd[n] := 0;
    end;

    for k := 1 to maxn do begin
        n := k;
        while n <= maxn do begin
            if length(d[n]) = nd[n] then setlength(d[n], 2*nd[n]);
            d[n][nd[n]] := k;
            inc(nd[n]);
            inc(n, k);
        end;
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        s[0] := 0;
        for i := 1 to n do s[i] := s[i-1] + a[i];

        ans := 0;
        for j := 0 to nd[n]-1 do begin

            k := d[n][j];
            t := n div k;
            mx := 0;
            mn := high(int64);

            for i := 1 to t do begin

                w := s[i*k] - s[i*k-k];
                mx := max(mx, w);
                mn := min(mn, w);

            end;

            ans := max(ans, mx - mn);
        end;
        writeln(ans);

    end;
end.

```
