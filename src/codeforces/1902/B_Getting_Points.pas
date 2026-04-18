program B_Getting_Points;
var
    ntc, tci: int16;
    n, l, t: int32;
    p, p2, p1, nt, d0, d1, x: int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n, p, l, t);

        p1 := l+t;
        p2 := p1+t;
        nt := (n+6) div 7;
        d0 := nt div 2;
        d1 := (nt+1) div 2;

        if d0 * p2 >= p then
            x := (p-1) div p2 + 1
        else begin
            dec(p, d1 * l + nt * t);
            if p <= 0 then
                x := d1
            else
                x := d1 + (p-1) div l + 1;
        end;

        writeln(n-x);
    end;
end.
