# Задатак: A_Dot_Product.pas

```pascal
program A_Dot_Product;
uses
    math;
const
    nn = 200 * 1000;
    inf = 100 * 1000 * 1000;
var
    ntc, tci, n, i: int32;
    ga, gb, aa, bb, ax, bx, xx: int64;
    na, nb, h, mx, eps: extended;
    a, b, x: array [1 .. nn] of int64;
    a1, b1, x1: array [1 .. nn] of extended;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
    h := 0.5;
    eps := 1E-18;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        ga := 0;
        gb := 0;
        aa := 0;
        bb := 0;

        for i := 1 to n do begin
            read(a[i]);
            ga := gcd(ga, a[i]);
        end;
        readln;

        for i := 1 to n do begin
            read(b[i]);
            gb := gcd(gb, b[i]);
        end;
        readln;

        for i := 1 to n do begin
            a[i] := a[i] div ga;
            b[i] := b[i] div gb;
            inc(aa, sqr(a[i]));
            inc(bb, sqr(b[i]));
        end;

        na := sqrt(aa);
        nb := sqrt(bb);
        mx := 0;

        for i := 1 to n do begin
            a1[i] := a[i] / na;
            b1[i] := b[i] / nb;
            x1[i] := a1[i] - b1[i];
            if mx < abs(x1[i]) then mx := abs(x1[i]);
        end;

        if mx > eps then begin
            xx := 0;
            ax := 0;
            bx := 0;

            for i := 1 to n do begin
                x[i] := floor(x1[i] / mx * inf + h);
                inc(xx, sqr(x[i]));
                inc(ax, a[i] * x[i]);
                inc(bx, b[i] * x[i]);
            end;
        end;

        if (mx > eps) and (ax > 0) and (bx < 0) then begin
            writeln('Yes');
            for i := 1 to n-1 do write(x[i], ' ');
            writeln(x[n]);
        end else
            writeln('No');

    end;
end.

```
