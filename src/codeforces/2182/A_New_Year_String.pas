program A_New_Year_String;
uses
	math;
var
	notc, tci, n, i, c, ans: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		c := 0;
		for i := 4 to n do begin
			if s[i-3] <> '2' then
			else if s[i-2] <> '0' then
			else if s[i-1] <> '2' then
			else if s[i-0] = '5' then
				inc(c);
		end;

		ans := c;
		for i := 4 to n do begin
			c := 0;
			if s[i-3] <> '2' then inc(c);
			if s[i-2] <> '0' then inc(c);
			if s[i-1] <> '2' then inc(c);
			if s[i-0] <> '6' then inc(c);
			ans := min(ans, c);
		end;

		writeln(ans);

	end;
end.
