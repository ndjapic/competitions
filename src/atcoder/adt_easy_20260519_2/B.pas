program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	n, i, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	ans := 0;
	for i := 1 to n do
		case s[i] of
			'v': inc(ans);
			'w': inc(ans, 2);
		end;

	writeln(ans);
end.
