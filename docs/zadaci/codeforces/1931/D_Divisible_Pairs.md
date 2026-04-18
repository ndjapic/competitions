# Задатак: D_Divisible_Pairs.pas

```pascal
program D_Divisible_Pairs;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, x, y, l, r, l0, r0: int32;
    ans: int64;
    a, merge, mx, my: array [1 .. maxn] of int32;

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
            if (k = r) or (j < m) and (
                (a[j] mod y < a[k] mod y) or
                (a[j] mod y = a[k] mod y) and
                (a[j] mod x <= a[k] mod x)
            ) then begin
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

        readln(n, x, y);

        for i := 1 to n do begin
            read(a[i]);
        end;
        readln;
        msort(1, n+1);

        for i := 1 to n do begin
            mx[i] := a[i] mod x;
            my[i] := a[i] mod y;
        end;

        l := 1;
        ans := 0;
        for i := 1 to n do
            if (i = n) or (my[i] < my[i+1]) then begin

                l0 := l;
                while (l <= i) and (mx[l] = 0) do inc(l);
                inc(ans, int64(l-l0-1) * (l-l0) div 2);

                r := i;
                while l < r do
                    if mx[l] + mx[r] < x then
                        inc(l)
                    else if mx[l] + mx[r] > x then
                        dec(r)
                    else if mx[l] < mx[r] then begin
                        l0 := l;
                        r0 := r;
                        while mx[l] = mx[l0] do inc(l);
                        while mx[r] = mx[r0] do dec(r);
                        inc(ans, int64(l-l0) * (r0-r));
                    end else begin
                        inc(ans, int64(r-l+1) * (r-l) div 2);
                        l := r;
                    end;
                l := i+1;

            end;

        writeln(ans);

    end;
end.

```
