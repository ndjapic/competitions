program gcd;

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
end.
