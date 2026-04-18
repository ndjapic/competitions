program B_Line_Segments;
uses
    math;
var
    ntc, tci, n, i: int32;
    ai, px, py, qx, qy, d2, s, mx: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(px, py, qx, qy);

        s := 0;
        mx := 0;
        for i := 1 to n do begin
            read(ai);
            inc(s, ai);
            mx := max(mx, ai);
        end;
        readln;

        d2 := sqr(px-qx) + sqr(py-qy);
        if d2 > sqr(s) then
            writeln('No')
        else if mx <= s - mx then
            writeln('Yes')
        else if d2 < sqr(s - 2*mx) then
            writeln('No')
        else
            writeln('Yes');

    end;
end.
