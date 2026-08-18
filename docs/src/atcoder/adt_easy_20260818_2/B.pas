program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #hashset
var
	n: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);

	if n mod 111 = 0 then
		writeln('Yes')
	else
		writeln('No');
end.
