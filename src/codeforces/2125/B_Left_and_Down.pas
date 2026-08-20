program B_Left_and_Down;
var
	ntc, tci: int32;
	a, b, k, g: int64;

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

		readln(a, b, k);

		g := gcd(a, b);
		a := a div g;
		b := b div g;

		if (a <= k) and (b <= k) then
			writeln(1)
		else
			writeln(2);

	end;
end.
