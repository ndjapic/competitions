program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	n = 12;
var
	i, ans: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	ans := 0;
	for i := 1 to n do begin
		readln(s);
		if length(s) = i then inc(ans);
	end;

	writeln(ans);
end.
