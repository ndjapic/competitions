# Задатак: euler125.pas

```pascal
program Palindromic_sums;
uses
    math;
var
    ntc, tci: int8;
    n, d, t, i: int32;
    l, r, s, ans: int64;
    digits: array [1 .. 18] of int8;
    a, merge: array of int32;

function is_pal(s: int32): boolean;
var
    l, r: int8;
begin
    r := 0;
    while s > 0 do begin
        inc(r);
        digits[r] := s mod 10;
        s := s div 10;
    end;

    l := 1;
    while (l < r) and (digits[l] = digits[r]) do begin
        inc(l);
        dec(r);
    end;
    is_pal := l >= r;
end;

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

        readln(n, d);

        t := 0;
        setlength(a, 1);

        l := 1;
        r := l+d;
        s := sqr(l) + sqr(r);
        while s < n do begin

            while s < n do begin

                if is_pal(s) then begin
                    if length(a) = t then setlength(a, 2*t);
                    a[t] := s;
                    inc(t);
                end;

                inc(r, d);
                inc(s, sqr(r));

            end;

            inc(l);
            r := l+d;
            s := sqr(l) + sqr(r);

        end;

        ans := 0;
        if t > 0 then begin
            setlength(merge, t);
            msort(0, t);
            ans := a[0];
            for i := 1 to t-1 do
                if a[i] > a[i-1] then inc(ans, a[i]);
        end;

        writeln(ans);

    end;
end.

```
