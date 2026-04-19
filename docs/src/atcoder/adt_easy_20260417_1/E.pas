program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	n, i, l, r, a, b: int32;
	cost, ans: int64;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);
	readln(s);

	setlength(s, 2*n);
	ans := int64(1) shl 60;

	for i := 0 to n-2 do begin
		s[i+1+n] := s[i+1];
		cost := int64(i) * a;
		if cost < ans then begin

			l := i + 1;
			r := i + n;
			while l < r do begin
				if s[l] <> s[r] then inc(cost, b);
				inc(l);
				dec(r);
			end;
			ans := min(ans, cost);

		end;
	end;

	writeln(ans);
end.
