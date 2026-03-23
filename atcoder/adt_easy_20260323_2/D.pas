program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x: int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x);

	if x >= 0 then
		x := (x+9) div 10
	else begin
		x := -((-x) div 10)
	end;

	writeln(x);
end.
