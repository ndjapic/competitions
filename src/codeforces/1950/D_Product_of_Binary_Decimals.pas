program D_Product_of_Binary_Decimals;
const
    maxn = 100 * 1000;
var
    ntc, tci: int32;
    n, p: int32;
    m, x, t: int8;
    bd: array [1 .. 33] of int32;
    d: array [1 .. 6] of int8;
    pobd: array [1 .. maxn] of boolean;

begin
    for m := 1 to 33 do begin

        x := m;
        t := 0;
        while x > 0 do begin
            inc(t);
            d[t] := x mod 2;
            x := x div 2;
        end;

        bd[m] := 0;
        while t > 0 do begin
            bd[m] := 10 * bd[m] + d[t];
            dec(t);
        end;

    end;

    for n := 1 to maxn do pobd[n] := false;
    pobd[1] := true;

    for n := 1 to maxn do
        if pobd[n] then begin
            m := 1;
            p := n * bd[m];
            while p <= maxn do begin
                pobd[p] := true;
                inc(m);
                p := n * bd[m];
            end;
        end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        if pobd[n] then
            writeln('YES')
        else
            writeln('NO');

    end;
end.
