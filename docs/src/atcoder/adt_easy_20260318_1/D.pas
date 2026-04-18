program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, y, z: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y, z);

	if abs(y) > abs(x) then
		writeln(abs(x))
	else begin
		if x < 0 then begin
			x := -x;
			y := -y;
			z := -z;
		end;

		if (y > x) or (y < 0) then
			writeln(x)
		else if z > y then
			writeln(-1)
		else
			writeln(abs(z) + abs(x-z));
	end;
end.
