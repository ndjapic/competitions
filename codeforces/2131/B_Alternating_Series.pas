program B_Alternating_Series;
uses
	math;
var
	ntc, tci: int16;
	n, i: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		if n = 2 then
			writeln('-1 2')
		else begin

			for i := 1 to n do begin
				if odd(i) then
					write('-1')
				else if i < n then
					write('3')
				else
					write('2');
				if i < n then write(' ');
			end;
			writeln;

		end;

	end;
end.
