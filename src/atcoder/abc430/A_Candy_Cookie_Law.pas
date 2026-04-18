program A_Candy_Cookie_Law;
var
	a, b, c, d: int8;

begin
	readln(a, b, c, d);

	if (c >= a) and (d < b) then
		writeln('Yes')
	else
		writeln('No');
end.
