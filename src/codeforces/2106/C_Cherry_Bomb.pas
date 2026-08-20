program C_Cherry_Bomb;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, k, x, l, r: int32;
    a, b: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(a[i]); readln;
        for i := 1 to n do read(b[i]); readln;

        i := 1;
        while (i <= n) and (b[i] = -1) do inc(i);

        if i <= n then begin

            x := a[i] + b[i];

            while (i <= n) and ((b[i] = -1) or (a[i] + b[i] = x)) do
                inc(i);

            if i <= n then
                writeln(0)
            else begin

                i := 1;
                while (i <= n) and (0 <= x - a[i]) and (x - a[i] <= k) do
                    inc(i);

                if i <= n then
                    writeln(0)
                else
                    writeln(1);

            end;

        end else begin

            l := a[1];
            r := a[1];
            for i := 2 to n do begin
                l := min(l, a[i]);
                r := max(r, a[i]);
            end;

            writeln(k+1-(r-l));

        end;

    end;
end.
