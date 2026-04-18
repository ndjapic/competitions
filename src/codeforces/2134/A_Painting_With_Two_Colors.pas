program A_Painting_With_Two_Colors;
uses
	math;
const
	nn = 200 * 1000;
var
	ntc, tci: int16;
	n, a, b: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, a, b);

		if odd(n-b) then
			writeln('NO')
		else if a <= b then
			writeln('YES')
		else if odd(n-a) then
			writeln('NO')
		else
			writeln('YES');

	end;
end.
