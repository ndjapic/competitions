program C_Cake_Assignment;
var
	ntc, tci: int32;
	k: int8;
	x: int64;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(k, x);

		while not odd(x) do begin
			x := x div 2;
			dec(k);
		end;
		x := x div 2;

		writeln(k);
		while k > 0 do begin
			write(x mod 2 + 1);
			if k > 1 then write(' ');
			x := x div 2;
			dec(k);
		end;
		writeln;

	end;
end.
