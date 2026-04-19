# Problem: B1_Bouquet.pas

```pascal
program B1_Bouquet;
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, {k,} l, r: int32;
    m, s, ans: int64;
    a, merge{, b, c}: array [0 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;
begin
    i := lend + 1;
    while (i < rend) and (a[i-1] <= a[i]) do inc(i);

    if i < rend then begin

        m := (lend + rend) div 2;
        {if i < m then} msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and (a[l] <= a[r]) then begin
                merge[i] := a[l];
                inc(l);
            end else begin
                merge[i] := a[r];
                inc(r);
            end;

        for i := lend to rend - 1 do a[i] := merge[i];

    end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        for i := 1 to n do read(a[i]); readln; msort(1, n+1);

        {b[1] := a[1];
        c[1] := 1;
        k := 1;

        for i := 2 to n do begin
            if a[i-1] < a[i] then begin
                inc(k);
                b[k] := a[i];
                c[k] := 0;
            end;
            inc(c[k]);
        end;

        l := 1;
        r := 1;
        s := int64(b[1]) * c[1];
        ans := 0;

        while r <= k do
            if s - int64(b[l]) * c[l] + 1 <= m then begin
                ans := max(ans, min(s, m));
                inc(r);
                inc(s, int64(b[r]) * c[r]);
                if b[r] - b[r-1] > 1 then begin
                    l := r;
                    s := c[r];
                end;
            end else begin
                dec(s, int64(b[l]) * c[l]);
                inc(l);
            end;}

        l := 1;
        r := 0;
        s := 0;
        ans := 0;

        while r <= n do
            if (s > m) or (a[r] - a[l] > 1) then begin
                dec(s, a[l]);
                inc(l);
            end else begin
                ans := max(ans, s);
                inc(r);
                inc(s, a[r]);
            end;

        writeln(ans);

    end;
end.

```
