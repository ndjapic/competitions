program B_Two_Divisors;
var
    ntc, tci: int16;
    a, b, x: int32;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

function lcm(a, b: int64): int64;
begin
    lcm := a div gcd(a, b) * b;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(a, b);

        if b mod a = 0 then
            x := b div a * b
        else
            x := lcm(b, a);

        writeln(x);

    end;
end.
