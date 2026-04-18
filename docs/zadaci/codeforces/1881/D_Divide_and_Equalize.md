# Задатак: D_Divide_and_Equalize.pas

```pascal
program D_Divide_and_Equalize;
uses
    math;
const
    maxn = 10 * 1000;
    maxx = 1000 * 1000;
var
    ntc, tci: int32;
    n, i, x, p, np: int32;
    a: array [1 .. maxn] of int32;
    lpd, c: array [2 .. maxx] of int32;
    primes: array [1 .. 78498] of int32;

begin
    for x := 1 to maxx do lpd[x] := x;

    p := 2;
    x := p*p;
    while x <= maxx do begin
        if lpd[p] = p then
            while x <= maxx do begin
                if lpd[x] = x then lpd[x] := p;
                inc(x, p);
            end;
        inc(p);
        x := p*p;
    end;

    np := 0;
    for p := 2 to maxx do
        if lpd[p] = p then begin
            inc(np);
            primes[np] := p;
            c[p] := 0;
        end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            read(a[i]);
            x := a[i];
            while x > 1 do begin
                p := lpd[x];
                inc(c[p]);
                x := x div p;
            end;
        end;
        readln;

        i := 1;
        p := 2;
        while (i <= np) and (c[p] mod n = 0) do begin
            inc(i);
            p := primes[i];
        end;

        if i <= np then
            writeln('NO')
        else
            writeln('YES');

        for i := 1 to n do begin
            x := a[i];
            while x > 1 do begin
                p := lpd[x];
                dec(c[p]);
                x := x div p;
            end;
        end;

    end;
end.

```
