program A_Maximum_Neighborhood;
uses
	math;
var
	notc, tci, n, ans: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		if n = 1 then
			ans := 1
		else if n = 2 then
			ans := 9
		else
			ans := max(
				4 * (sqr(n)-1) - n,
				5 * (sqr(n) - (n+1))
			);

		writeln(ans);

	end;
end.
