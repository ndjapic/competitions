# Задатак: euler049.pas

```pascal
program Prime_permutations;
const
    maxn = 1000 * 1000;
var
    n, p, x, l, r, dif, t, i: int32;
    k, d: int8;
    is_prime: array [2 .. maxn] of boolean;
    h, top, link: array [2 .. maxn] of int32;
    c: array [0 .. 9] of int8;
    b: array [1 .. maxn] of record
        l, r, d: int32;
    end;
    per, merge: array [1 .. maxn] of int32;

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
                (b[per[j]].l < b[per[k]].l) or
                (b[per[j]].l = b[per[k]].l) and
                (b[per[j]].d < b[per[k]].d)
            ) then begin
                merge[i] := per[j];
                inc(j);
            end else begin
                merge[i] := per[k];
                inc(k);
            end;

        for i := l to r-1 do per[i] := merge[i];

    end;
end;

begin
    for p := 2 to maxn do is_prime[p] := true;
    for p := 2 to 1000 do
        if is_prime[p] then begin
            n := p*p;
            while n <= maxn do begin
                is_prime[n] := false;
                inc(n, p);
            end;
        end;

    for x := 2 to maxn do top[x] := 0;
    for d := 0 to 9 do c[d] := 0;

    for p := 2 to maxn do
        if is_prime[p] then begin

            x := p;
            while x > 0 do begin
                inc(c[x mod 10]);
                x := x div 10;
            end;

            for d := 9 downto 0 do
                while c[d] > 0 do begin
                    x := x * 10 + d;
                    dec(c[d]);
                end;

            h[p] := x;
            link[p] := top[x];
            top[x] := p;

        end;

    readln(n, k);

    t := 0;
    for x := 2 to maxn do begin

        r := top[x];
        while r > 0 do begin
            l := link[r];
            while l > 0 do begin
                if (l < n) and ((r-l) mod (k-1) = 0) then begin

                    dif := (r-l) div (k-1);

                    p := l + dif;
                    while (p < r) and is_prime[p] and (h[p] = x) do
                        inc(p, dif);

                    if p = r then begin
                        inc(t);
                        b[t].l := l;
                        b[t].r := r;
                        b[t].d := dif;
                        per[t] := t;
                    end;

                end;
                l := link[l];
            end;
            r := link[r];
        end;

    end;

    msort(1, t+1);

    for i := 1 to t do begin

        p := b[per[i]].l;
        while p <= b[per[i]].r do begin
            write(p);
            inc(p, b[per[i]].d);
        end;
        writeln;

    end;

end.

```
