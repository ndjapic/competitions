program C_Maximum_Even_Sum;
var
	ntc, tci: int32;
	a, b: int64;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(a, b);

		if odd(b) then begin
			if odd(a) then
				writeln(a*b+1)
			else
				writeln(-1);
		end else if odd(b div 2) then begin
			if odd(a) then
				writeln(-1)
			else
				writeln(b div 2 * a + 2);
		end else
			writeln(b div 2 * a + 2);

	end;
end.
