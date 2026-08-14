program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	s, t: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function d(s: string): int8;
begin
	result := abs(ord(s[1]) - ord(s[2]));
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	Randomize;

	readln(s);
	readln(t);

	if (d(s) = d(t)) or (d(s) + d(t) = 5) then
		writeln('Yes')
	else
		writeln('No');
end.
