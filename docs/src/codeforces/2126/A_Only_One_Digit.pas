program A_Only_One_Digit;
uses
	math;
var
	ntc, tci, x, y: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(x);

		y := 9;
		while x > 0 do begin
			y := min(y, x mod 10);
			x := x div 10;
		end;

		writeln(y);

	end;
end.
