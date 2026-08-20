program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 1000 * 1000;
var
	s: string;
	n, i, score: int32;
	ans: int64;
	c: array [-nn .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	readln(s);

	for score := -n to n do c[score] := 0;

	score := 0;
	c[0] := 1;
	ans := 0;
	for i := 1 to n do begin
		case s[i] of
			'V': inc(score);
			'F': dec(score);
		end;
		inc(ans, c[score]);
		inc(c[score]);
	end;

	writeln(ans);
end.
