program A_Blackboard_Game;
var
	ntc, tci, n: int8;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n);

		if n mod 4 = 0 then
			writeln('Bob')
		else
			writeln('Alice');

	end;
end.
