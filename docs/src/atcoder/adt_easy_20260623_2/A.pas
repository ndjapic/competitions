program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b, culprit: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	if a = b then
		culprit := -1
	else begin

		culprit := 1;
		while (culprit = a) or (culprit = b) do inc(culprit);

	end;

	writeln(culprit);
end.
