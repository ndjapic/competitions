program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int64;
	c: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	c := 0;
	while ((a > 0) or (b > 0)) and (c < 1) do begin
		c := (a mod 10 + b mod 10 + c) div 10;
		a := a div 10;
		b := b div 10;
	end;

	if c < 1 then
		writeln('Easy')
	else
		writeln('Hard');
end.
