program A_Too_Many_Requests;
var
	n, m, i: int8;

begin
	readln(n, m);

	for i := 1 to n do
		if i <= m then
			writeln('OK')
		else
			writeln('Too Many Requests');
end.
