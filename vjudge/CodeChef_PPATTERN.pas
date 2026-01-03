program CodeChef_PPATTERN;
const
    nn = 100;
var
    ntc, tci, n, i, j: int8;
    s, x: int16;
    a: array [1 .. nn, 1 .. nn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);

        x := 0;
        for s := 2 to n+n do
            for i := 1 to n do begin
                j := s-i;
                if (0 < j) and (j <= n) then begin
                    inc(x);
                    a[i, j] := x;
                end;
            end;

        for i := 1 to n do begin
            for j := 1 to n-1 do write(a[i, j], ' ');
            writeln(a[i, n]);
        end;
    end;
end.
