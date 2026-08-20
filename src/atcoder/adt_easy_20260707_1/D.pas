program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	N = 15;
	H = (N+1) div 2;
var
	r, c, d: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, c);

	d := max(abs(r - H), abs(c - H));

	if odd(d) then
		writeln('black')
	else
		writeln('white');
end.
