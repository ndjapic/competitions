program A_Yes_or_Yes;
var
	notc, tci, n, i, c: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(s);
		n := length(s);

		c := 0;
		for i := 1 to n do
			if s[i] = 'Y' then inc(c);

		if c <= 1 then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
