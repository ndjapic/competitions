# Problem: C_Permutation_Counting.pas

```pascal
program C_Permutation_Counting;
const
    sz = 200 * 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int8;
    n, i: int32;
    k, d, m, ans: int64;
    a, merge, s: array [0 .. sz] of int64;

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

        readln(n, k);
        for i := 1 to n do read(a[i]);
        readln;
        msort(1, n+1);
        a[0] := 0;
        a[n+1] := inf;

        s[0] := 0;
        for i := 1 to n do s[i] := s[i-1] + a[i];

        if s[n] + k >= a[n] * n then begin

            ans := k+s[n];

        end else begin

            i := n;
            while (i > 0) and (k < a[i] * i - s[i]) do dec(i);

            dec(k, a[i] * i - s[i]);
            d := k div i;
            m := k mod i;

            inc(d, a[i]);
            inc(m, n-i);
            ans := d*n+m;

        end;

        writeln(ans-n+1);

    end;
end.

```
