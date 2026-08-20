program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	ch: char;
	ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	ans := length(s);
	for ch in s do
		if ch = 'w' then inc(ans);

	writeln(ans);
end.
