program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	ans: int32;
	s: string;
	ch: char;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	ans := 0;
	for ch in s do
		case ch of
			'v': inc(ans);
			'w': inc(ans, 2);
		end;

	writeln(ans);
end.
