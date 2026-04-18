program D_Problem_about_GCD;
var
    ntc, tci: int16;
    l, r, g, a, b, d, i: int64;
    found: boolean;

function gcd(a, b: int64): int64;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(l, r, g);
        inc(l, g-1);

        a := l div g;
        b := r div g;

        d := -1;
        found := false;
        while (d < b-a) and not found do begin
            inc(d);
            i := -1;
            while (i < d) and not found do begin
                inc(i);
                found := gcd(a+i, b-d+i) = 1;
            end;
        end;

        if found then
            writeln((a+i)*g, ' ', (b-d+i)*g)
        else
            writeln('-1 -1');

    end;
end.
