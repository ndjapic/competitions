program C_Subset_Multiplication;
const
    nn = 600 * 1000;
var
    ntc, tci, n, i, x: int32;
    b: array [1 .. nn] of int32;

function gcd(a, b: int32): int32;
begin
    if b = 0 then
        gcd := a
    else
        gcd := gcd(b, a mod b);
end;

function lcm(a, b: int32): int32;
begin
    lcm := a div gcd(a, b) * b;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(b[i]);
        readln;

        x := 1;
        for i := 1 to n-1 do
            if b[i+1] mod b[i] > 0 then
                x := lcm(x, b[i] div gcd(b[i], b[i+1]));

        writeln(x);

    end;
end.
