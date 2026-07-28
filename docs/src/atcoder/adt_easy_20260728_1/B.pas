program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	r: int32;
	x: int8;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, x);

	case x of
		1: ans := (1600 <= r) and (r <= 2999);
		2: ans := (1200 <= r) and (r <= 2399);
	end;

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
