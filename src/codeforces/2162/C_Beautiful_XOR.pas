program C_Beautiful_XOR;
var
	notc, tci, a, b, p2: int32;
	x: array [1 .. 2] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(a, b);

		p2 := 1;
		while p2 <= a do p2 := 2 * p2;

		x[1] := (p2-1) xor a;
		x[2] := (p2-1) xor b;

		if x[2] > p2-1 then
			writeln(-1)
		else begin
			writeln(2);
			writeln(x[1], ' ', x[2]);
		end;

	end;
end.
