program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
var
	x, y, z, ans: int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(x, y, z);

	if x < 0 then begin
		x := -x;
		y := -y;
		z := -z;
	end;

	if (y < 0) or (y > x) then
		ans := x
	else if z > y then
		ans := -1
	else
		ans := abs(z) + abs(y-z) + abs(x-y);

	writeln(ans);
end.
