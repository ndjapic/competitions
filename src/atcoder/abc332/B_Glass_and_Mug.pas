program B_Glass_and_Mug;
uses
    math;
var
    k, i, g, m, x, y, d: int32;

begin
    readln(k, g, m);
    x := 0;
    y := 0;

    for i := 1 to k do
        if x = g then
            x := 0
        else if y = 0 then
            y := m
        else begin
            d := min(g-x, y);
            dec(y, d);
            inc(x, d);
        end;

    writeln(x, ' ', y);
end.
