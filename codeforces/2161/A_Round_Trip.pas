program A_Round_Trip;
{$MODE DELPHI}
uses
	math;
var
	notc, tci, n, i, x, r, d, ans: int32;
	s: string;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(r, x, d, n);
		readln(s);

		ans := 0;
		for i := 1 to n do
			if (r < x) or (s[i] = '1') then begin
				inc(ans);
				r := max(0, r-d);
			end;

		writeln(ans);

	end;
end.
