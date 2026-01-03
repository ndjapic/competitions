program C_Balls_and_Bag_Query;
const
    nn = 200 * 1000;
    xx = 1000 * 1000;
var
    q, i, x, d: int32;
    qt: int8;
    c: array [1 .. xx] of int32;

begin
    readln(q);
    d := 0;

    for x := 1 to xx do c[x] := 0;

    for i := 1 to q do begin

        read(qt);

        case qt of

            1: begin
                read(x);
                if c[x] = 0 then inc(d);
                inc(c[x]);
            end;

            2: begin
                read(x);
                dec(c[x]);
                if c[x] = 0 then dec(d);
            end;

            3: writeln(d);

        end;
        readln;

    end;
end.
