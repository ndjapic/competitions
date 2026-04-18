program C_Absolute_Zero;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
    kk = 40;
var
    ntc, tci: int16;
    n, i, mn, mx: int32;
    k: int8;
    a: array [1 .. nn] of int32;
    x: array [1 .. kk] of int32;

procedure op();
begin
    mn := a[1];
    mx := a[1];
    for i := 2 to n do begin
        mn := min(mn, a[i]);
        mx := max(mx, a[i]);
    end;

    inc(k);
    x[k] := (mx+mn) div 2;
    for i := 1 to n do a[i] := abs(a[i] - x[k]);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]);
        readln;

        k := 0;
        mx := high(int32);
        while (k <= kk) and (0 < mx) do op();
        dec(k);

        if k >= kk then
            writeln(-1)
        else begin
            writeln(k);
            for i := 1 to k-1 do write(x[i], ' ');
            if k > 0 then writeln(x[k]);
        end;

    end;
end.
