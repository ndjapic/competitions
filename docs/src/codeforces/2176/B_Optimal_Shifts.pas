program B_Optimal_Shifts;
uses
	math;
var
	notc, tci, n, i, l, r, ans: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		readln(s);

		setlength(s, 2*n);
		for i := 1 to n do s[i+n] := s[i];

		l := 1;
		ans := 0;
		for r := 1 to 2*n do
			if s[r] = '1' then begin
				ans := max(ans, r-l);
				l := r+1;
			end;

		writeln(ans);

	end;
end.
