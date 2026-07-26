program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, y, z, d: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y, z);

	if (x*y < 0) or (abs(y) > abs(x)) then
		d := abs(x)
	else if z*y < 0 then
		d := 2 * abs(z) + abs(x)
	else if abs(z) > abs(y) then
		d := -1
	else
		d := abs(x);

	writeln(d);
end.
