program A_Social_Experiment;
var
	notc, tci, n: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		if n < 4 then
			writeln(n)
		else
			writeln(n mod 2);

	end;
end.
