program A_Simple_Design;
var
    ntc, tci, x, k: int32;

function dsum(x: int32): int8;
begin
    if x = 0 then
        dsum := 0
    else
        dsum := x mod 10 + dsum(x div 10);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(x, k);

        while dsum(x) mod k > 0 do inc(x);

        writeln(x);

    end;
end.
