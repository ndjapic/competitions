program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, c: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b, c);

	if c > b then inc(b, 24);
	if c > a then inc(a, 24);

	if a < b then
		writeln('Yes')
	else
		writeln('No');
end.
