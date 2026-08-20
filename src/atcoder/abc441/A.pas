program A;
var
	p, q, x, y: int32;

begin
	readln(p, q);
	readln(x, y);

	if (p <= x) and (q <= y) and (x-p < 100) and (y-q < 100) then
		writeln('Yes')
	else
		writeln('No');
end.
