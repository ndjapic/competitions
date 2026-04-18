program A_Maple_and_Multiplication;
var
	ntc, tci, a, b: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(a, b);

		if a = b then
			writeln(0)
		else if (a mod b = 0) or (b mod a = 0) then
			writeln(1)
		else
			writeln(2);

	end;
end.
