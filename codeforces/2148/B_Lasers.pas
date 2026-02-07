program B_Lasers;
const
	nn = 200 * 1000;
var
	ntc, tci, n, m, x, y, i: int32;
	a, b: array [1 .. nn] of int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m, x, y);
		readln;
		readln;
		writeln(n + m);

	end;
end.
