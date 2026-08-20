program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	NN = 10 * 1000;
var
	s: string;
	n, i, l, r, lim, c: int32;
	ans: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	ans := 0;

	for r := 1 to n do begin
		l := r;
		i := 0;
		lim := min(l-1, n-r);
		c := 0;

		while (i <= lim) and (c < 2) do begin
			if s[l-i] <> s[r+i] then inc(c);
			if c < 2 then inc(ans);
			inc(i);
		end;
	end;

	for r := 2 to n do begin
		l := r-1;
		i := 0;
		lim := min(l-1, n-r);
		c := 0;

		while (i <= lim) and (c < 2) do begin
			if s[l-i] <> s[r+i] then inc(c);
			if c < 2 then inc(ans);
			inc(i);
		end;
	end;

	writeln(ans);
end.
