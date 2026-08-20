program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);

	if (s[1] <> s[2]) and (s[1] <> s[3]) then
		writeln(s[1])
	else if (s[2] <> s[1]) and (s[2] <> s[3]) then
		writeln(s[2])
	else if (s[3] <> s[1]) and (s[3] <> s[2]) then
		writeln(s[3])
	else
		writeln(-1);
end.
