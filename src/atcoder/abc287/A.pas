program A_Majority;
var
	n, i, f, a: int8;
	ch: char;

begin
	readln(n);

	f := 0;
	a := 0;

	for i := 1 to n do begin
		read(ch);
		case ch of
			'F': inc(f);
			'A': inc(a);
		end;
		readln;
	end;

	if f > a then
		writeln('Yes')
	else
		writeln('No');
end.
