program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r: int32;
	x: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, x);

	case x of

		1: if (1600 <= r) and (r < 3000) then
			writeln('Yes')
		else
			writeln('No');

		2: if (1200 <= r) and (r < 2400) then
			writeln('Yes')
		else
			writeln('No');

	end;
end.
