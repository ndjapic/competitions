program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 100;
var
	a, b: int64;
	c: int8;
	easy: boolean;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	easy := true;
	c := 0;
	while easy and (a > 0) and (b > 0) do begin
		c := a mod 10 + b mod 10 + c;
		easy := c < 10;
		a := a div 10;
		b := b div 10;
		c := c div 10;
	end;

	if easy then
		writeln('Easy')
	else
		writeln('Hard');
end.
