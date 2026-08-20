program _A;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
var
	r, g, b: int8;
	c: string;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(r, g, b);
	readln(c);

	if c[1] = 'R' then
		writeln(min(g, b))
	else if c[1] = 'G' then
		writeln(min(b, r))
	else if c[1] = 'B' then
		writeln(min(r, g));
end.
