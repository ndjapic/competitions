program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	p, q, x, y: int32;
	ans: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(p, q, x, y);

	dec(x, p);
	dec(y, q);

	ans := (0 <= x) and (x < 100) and (0 <= y) and (y < 100);

	if ans then
		writeln('Yes')
	else
		writeln('No');
end.
