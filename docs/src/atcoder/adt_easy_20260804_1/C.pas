program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	a, b: int64;
	d: int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(a, b);

	d := 0;
	while (d < 10) and ((a > 0) or (b > 0)) do begin
		d := a mod 10 + b mod 10 + d div 10;
		a := a div 10;
		b := b div 10;
	end;

	if d < 10 then
		writeln('Easy')
	else
		writeln('Hard');
end.
