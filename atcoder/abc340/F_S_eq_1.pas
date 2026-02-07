program F_S_eq_1;
uses
    math;
var
    x, y, a, b, g: int64;

procedure egcd(var a, b, g: int64; x, y: int64);
var
    q, k, c, d: int64;
begin
    if y = 0 then begin
        a := 1;
        b := 0;
        g := x;
    end else begin
        q := x div y;
        egcd(c, d, g, y, x-y*q);
        (* ax-by=g *)
        (* a(qy+r)-by=g *)
        (* (aq-b)y+ar=g *)
        (* cy-dr=g *)
        (* c=aq-b, d=-a *)
        (* a=-d, b=aq-c *)
        k := (d+y-1) div y;
        a := k*y-d;
        b := k*x+a*q-c;
    end;
end;

begin
    readln(x, y);
    egcd(a, b, g, abs(x), abs(y));
    if g > 2 then
        writeln(-1)
    else begin
        if x < 0 then a := -a;
        if y < 0 then b := -b;
        if g = 1 then begin
            a := 2*a;
            b := 2*b;
        end;
        writeln(a, ' ', b);
    end;
end.
