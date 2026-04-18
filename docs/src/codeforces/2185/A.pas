program A;
var
	notc, tci, n, i: int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		for i := 1 to n-1 do write(i, ' ');
		writeln(n);

	end;
end.
