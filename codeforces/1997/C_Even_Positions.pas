program C_Even_Positions;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, h: int32;
    cost: int64;
    s: string;
    a: array [0 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        h := 0;
        for i := 1 to n do
            if s[i] = '_' then begin
                if h = 0 then begin
                    s[i] := '(';
                    inc(h);
                end else begin
                    s[i] := ')';
                    dec(h);
                end;
            end else begin
                if s[i] = '(' then
                    inc(h)
                else
                    dec(h);
            end;

        cost := 0;
        for i := 1 to n do
            if s[i] = '(' then begin
                inc(h);
                a[h] := i;
            end else begin
                inc(cost, i-a[h]);
                dec(h);
            end;

        writeln(cost);

    end;
end.
