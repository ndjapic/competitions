program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	sysutils;
var
	n: int32;
	s: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(s);
	n := strtoint(rightstr(s, 3));

	if (0 < n) and (n < 350) and (n <> 316) then
		writeln('Yes')
	else
		writeln('No');
end.
