program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i, a, l, r: int8;
	s: char;
	ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	l := 0;
	r := 0;
	ans := 0;

	for i := 1 to n do begin
		readln(a, s, s);
		case s of

			'L': if l = 0 then
				l := a
			else begin
				inc(ans, abs(a - l));
				l := a;
			end;

			'R': if r = 0 then
				r := a
			else begin
				inc(ans, abs(a - r));
				r := a;
			end;

		end;
	end;

	writeln(ans);
end.
