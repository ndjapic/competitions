program B_Maximum_Multiple_Sum;
{$mode objfpc}{$H+}{$J-}
var
    ntc, tci, n, i, x: int8;
    s, m, t: int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        m := 0;
        for i := 2 to n do begin

            s := 0;
            t := i;
            while t <= n do begin
                inc(s, t);
                inc(t, i);
            end;

            if m < s then begin
                m := s;
                x := i;
            end;

        end;

        writeln(x);

    end;
end.
