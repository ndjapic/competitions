program A_Approximation;
var
    a, b, c, d: int32;

begin
    readln(a, b);

    d := a div b;
    c := d * b;

    if a-c > c+b-a then inc(d);

    writeln(d);
end.
