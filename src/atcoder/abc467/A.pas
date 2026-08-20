program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	h, w: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(h, w);

	if w * sqr(100) >= sqr(h) * 25 then
		writeln('Yes')
	else
		writeln('No');
end.
