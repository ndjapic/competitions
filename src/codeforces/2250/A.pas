program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	INF = 1000 * 1000 * 1000;
var
	notc, tci, n, i, w, l, r: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(notc);
	for tci := 1 to notc do begin

		readln(n);

		l := INF;
		r := 1;

		for i := 1 to n do begin
			read(w);
			if odd(i) then
				l := min(l, w)
			else
				r := max(r, w);
		end;
		readln;

		if not odd(n) and (l-r > 1) then
			writeln('YES')
		else
			writeln('NO');

	end;
end.
