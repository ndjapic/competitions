program D_Cubes;
uses
    math;
const
    nn = 300 * 1000;
var
    n, d, d3, x, y, xy, s2, s: int64;
    found: boolean;

function isqrt(a: int64): int64;
var
    x: int64;
begin
    x := min(a, high(int32));
    while x * x > a do
        x := (x + a div x) div 2;
    isqrt := x;
end;

begin
    readln(n);

    d := 1;
    d3 := 1;
    found := false;

    while (d3 < n) and not found do begin
        xy := n-d3;

        if xy mod (3*d) = 0 then begin

            xy := xy div (3*d);
            s2 := sqr(d) + 4*xy;
            s := isqrt(s2);
            found := sqr(s) = s2;

            if found then begin
                x := (s+d) div 2;
                y := (s-d) div 2;
            end;

        end;

        if not found then begin
            inc(d);
            d3 := d*d*d;
        end;
    end;

    if found then
        writeln(x, ' ', y)
    else
        writeln(-1);
end.
