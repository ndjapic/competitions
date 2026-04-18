program B_ARC_Division;
var
    n, i, d: int8;
    r, a: int32;

begin
    readln(n, r);

    for i := 1 to n do begin
        readln(d, a);
        case d of
            1: if (1600 <= r) and (r < 2800) then inc(r, a);
            2: if (1200 <= r) and (r < 2400) then inc(r, a);
        end;
    end;

    writeln(r);
end.
