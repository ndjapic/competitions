program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	n, i: int8;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := length(s);

	for i := 1 to n do
		if (s[i] = '.') and ((i = 1) or (s[i-1] = '#')) then
			s[i] := 'o';

	writeln(s);
end.
