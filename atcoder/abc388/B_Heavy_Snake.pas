program B_Heavy_Snake;
uses
    math;
const
    nn = 100;
var
    n, d, k, i: int8;
    mx: int32;
    t, l: array [1 .. nn] of int32;

begin
    readln(n, d);

    for i := 1 to n do readln(t[i], l[i]);

    for k := 1 to d do begin
        mx := 0;
        for i := 1 to n do begin
            inc(l[i]);
            mx := max(mx, t[i]*l[i]);
        end;
        writeln(mx);
    end;
end.
