program A_Greedy_Grid;
uses
	math;
var
	ntc, tci, n, m: int32;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, m);

		if min(n, m) < 2 then
			writeln('NO')
		else if max(n, m) = 2 then
			writeln('NO')
		else
			writeln('YES');

	end;
end.
