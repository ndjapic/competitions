program A_Add_or_XOR;
uses
    math;
var
    ntc, tci, a, b, x, y, cost: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(a, b, x, y);

        if odd(a) and (a-b = 1) then
            cost := y
        else if a > b then
            cost := -1
        else begin
            cost := 0;
            while a < b do begin
                if odd(a) then
                    inc(cost, x)
                else
                    inc(cost, min(x, y));
                inc(a);
            end;
        end;

        writeln(cost);

    end;
end.
